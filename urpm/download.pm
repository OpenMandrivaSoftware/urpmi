package urpm::download;

# $Id$

use strict;
use urpm::msg;
use urpm::util;
use Cwd;
use Exporter;

our @ISA = 'Exporter';
our @EXPORT = qw(get_proxy
	propagate_sync_callback
	sync_file sync_rsync sync_ssh
	set_proxy_config dump_proxy_config
);

(our $VERSION) = q($Revision$) =~ /(\d+)/;

#- proxy config file.
our $PROXY_CFG = '/etc/urpmi/proxy.cfg';
my $proxy_config;

#- Timeout for curl connection and wget operations
our $CONNECT_TIMEOUT = 60; #-  (in seconds)



sub ftp_http_downloaders() { qw(curl wget prozilla) }

sub available_ftp_http_downloaders() {
    my %binaries = (
	curl => 'curl', 
	wget => 'wget', 
	prozilla => 'proz',
    );
    grep { -x "/usr/bin/$binaries{$_}" || -x "/bin/$binaries{$_}" } ftp_http_downloaders();
}


#- parses proxy.cfg (private)
sub load_proxy_config () {
    return if defined $proxy_config;
    $proxy_config = {};
    foreach (cat_($PROXY_CFG)) {
	chomp; s/#.*$//; s/^\s*//; s/\s*$//;
	if (/^(?:(.*):\s*)?(ftp_proxy|http_proxy)\s*=\s*(.*)$/) {
	    $proxy_config->{$1 || ''}{$2} = $3;
	    next;
	}
	if (/^(?:(.*):\s*)?proxy_user\s*=\s*([^:]*)(?::(.*))?$/) {
	    $proxy_config->{$1 || ''}{user} = $2;
	    $proxy_config->{$1 || ''}{pwd} = $3 if defined $3;
	    next;
	}
	if (/^(?:(.*):\s*)?proxy_user_ask/) {
	    $proxy_config->{$1 || ''}{ask} = 1;
	    next;
	}
    }
}

#- writes proxy.cfg
sub dump_proxy_config () {
    return 0 unless defined $proxy_config; #- hasn't been read yet
    open my $f, '>', $PROXY_CFG or return 0;
    foreach ('', sort grep { !/^(|cmd_line)$/ } keys %$proxy_config) {
	my $m = $_ eq '' ? '' : "$_:";
	my $p = $proxy_config->{$_};
	foreach (qw(http_proxy ftp_proxy)) {
	    defined $p->{$_} && $p->{$_} ne ''
		and print $f "$m$_=$p->{$_}\n";
	}
	if ($p->{ask}) {
	    print $f "${m}proxy_user_ask\n";
	    next;
	}
	defined $p->{user} && $p->{user} ne ''
	    and print $f "${m}proxy_user=$p->{user}:$p->{pwd}\n";
    }
    close $f;
    chmod 0600, $PROXY_CFG; #- may contain passwords
    return 1;
}

#- deletes the proxy configuration for the specified media
sub remove_proxy_media {
    defined $proxy_config and delete $proxy_config->{$_[0] || ''};
}

#- reads and loads the proxy.cfg file ;
#- returns the global proxy settings (without arguments) or the
#- proxy settings for the specified media (with a media name as argument)
sub get_proxy (;$) {
    my ($o_media) = @_; $o_media ||= '';
    load_proxy_config();
    my $p = $proxy_config->{cmd_line}
	|| $proxy_config->{$o_media}
	|| $proxy_config->{''}
	|| {
	    http_proxy => undef,
	    ftp_proxy => undef,
	    user => undef,
	    pwd => undef,
	};
    if ($p->{ask} && ($p->{http_proxy} || $p->{ftp_proxy}) && !$p->{user}) {
	our $PROMPT_PROXY;
	unless (defined $PROMPT_PROXY) {
	    require urpm::prompt;
	    $PROMPT_PROXY = new urpm::prompt(
		N("Please enter your credentials for accessing proxy\n"),
		[ N("User name:"), N("Password:") ],
		undef,
		[ 0, 1 ],
	    );
	}
	($p->{user}, $p->{pwd}) = $PROMPT_PROXY->prompt;
    }
    $p;
}

#- copies the settings for proxies from the command line to media named $media
#- and writes the proxy.cfg file (used when adding new media)
sub copy_cmd_line_proxy {
    my ($media) = @_;
    return unless $media;
    load_proxy_config();
    if (defined $proxy_config->{cmd_line}) {
	$proxy_config->{$media} = $proxy_config->{cmd_line};
	dump_proxy_config();
    } else {
	#- use default if available
	$proxy_config->{$media} = $proxy_config->{''};
    }
}

#- overrides the config file proxy settings with values passed via command-line
sub set_cmdline_proxy {
    my (%h) = @_;
    load_proxy_config();
    $proxy_config->{cmd_line} ||= {
	http_proxy => undef,
	ftp_proxy => undef,
	user => undef,
	pwd => undef,
    };
    $proxy_config->{cmd_line}{$_} = $h{$_} foreach keys %h;
}

#- changes permanently the proxy settings
sub set_proxy_config {
    my ($key, $value, $o_media) = @_;
    $proxy_config->{$o_media || ''}{$key} = $value;
}

#- set up the environment for proxy usage for the appropriate tool.
#- returns an array of command-line arguments for wget or curl.
sub set_proxy {
    my ($proxy) = @_;
    my @res;
    if (defined $proxy->{proxy}{http_proxy} || defined $proxy->{proxy}{ftp_proxy}) {
	for ($proxy->{type}) {
	    /\bwget\b/ and do {
		for ($proxy->{proxy}) {
		    if (defined $_->{http_proxy}) {
			$ENV{http_proxy} = $_->{http_proxy} =~ /^http:/
			    ? $_->{http_proxy}
			    : "http://$_->{http_proxy}";
		    }
		    $ENV{ftp_proxy} = $_->{ftp_proxy} if defined $_->{ftp_proxy};
		    @res = ("--proxy-user=$_->{user}", "--proxy-passwd=$_->{pwd}")
			if defined $_->{user} && defined $_->{pwd};
		}
		last;
	    };
	    /\bcurl\b/ and do {
		for ($proxy->{proxy}) {
		    push @res, ('-x', $_->{http_proxy}) if defined $_->{http_proxy};
		    push @res, ('-x', $_->{ftp_proxy}) if defined $_->{ftp_proxy};
		    push @res, ('-U', "$_->{user}:$_->{pwd}")
			if defined $_->{user} && defined $_->{pwd};
		}
		last;
	    };
	    die N("Unknown webfetch `%s' !!!\n", $proxy->{type});
	}
    }
    return @res;
}

sub _error {
    my ($name) = @_;

    my $msg = $? & 127 ? N("%s failed: exited with signal %d", $name, $? & 127) :
                         N("%s failed: exited with %d", $name, $? >> 8);
    die "$msg\n";
}

sub propagate_sync_callback {
    my $options = shift;
    if (ref($options) && $options->{callback}) {
	my $mode = shift;
	if ($mode =~ /^(?:start|progress|end)$/) {
	    my $file = shift;
	    $file =~ s|([^:]*://[^/:\@]*:)[^/:\@]*(\@.*)|$1xxxx$2|; #- if needed...
	    return $options->{callback}($mode, $file, @_);
	} else {
	    return $options->{callback}($mode, @_);
	}
    }
}

sub sync_file {
    my $options = shift;
    foreach (@_) {
	propagate_sync_callback($options, 'start', $_);
	require urpm::util;
	urpm::util::copy($_, ref($options) ? $options->{dir} : $options)
	    or die N("copy failed");
	propagate_sync_callback($options, 'end', $_);
    }
}

sub sync_wget {
    -x "/usr/bin/wget" or die N("wget is missing\n");
    my $options = shift;
    $options = { dir => $options } if !ref $options;
    #- force download to be done in cachedir to avoid polluting cwd.
    (my $cwd) = getcwd() =~ /(.*)/;
    chdir $options->{dir};
    my ($buf, $total, $file) = ('', undef, undef);
    my $wget_command = join(" ", map { "'$_'" }
	#- construction of the wget command-line
	"/usr/bin/wget",
	($options->{'limit-rate'} ? "--limit-rate=$options->{'limit-rate'}" : ()),
	($options->{resume} ? "--continue" : "--force-clobber"),
	($options->{proxy} ? set_proxy({ type => "wget", proxy => $options->{proxy} }) : ()),
	($options->{retry} ? ('-t', $options->{retry}) : ()),
	($options->{callback} ? ("--progress=bar:force", "-o", "-") :
	    $options->{quiet} ? "-q" : @{[]}),
	"--retr-symlinks",
	"--no-check-certificate",
	"--timeout=$CONNECT_TIMEOUT",
	(defined $options->{'wget-options'} ? split /\s+/, $options->{'wget-options'} : ()),
	'-P', $options->{dir},
	@_
    ) . " |";
    $options->{debug} and $options->{debug}($wget_command);
    my $wget_pid = open(my $wget, $wget_command);
    local $/ = \1; #- read input by only one char, this is slow but very nice (and it works!).
    local $_;
    while (<$wget>) {
	$buf .= $_;
	if ($_ eq "\r" || $_ eq "\n") {
	    if ($options->{callback}) {
		if ($buf =~ /^--\d\d:\d\d:\d\d--\s+(\S.*)\n/ms) {
		    if ($file && $file ne $1) {
			propagate_sync_callback($options, 'end', $file);
			undef $file;
		    }
		    ! defined $file and propagate_sync_callback($options, 'start', $file = $1);
		} elsif (defined $file && ! defined $total && $buf =~ /==>\s+RETR/) {
		    $total = '';
		} elsif (defined $total && $total eq '' && $buf =~ /^[^:]*:\s+(\d\S*)/) {
		    $total = $1;
		} elsif (defined $total && $buf =~ /^\s*(\d+)%.*\s+(\S+)\s+ETA\s+(\S+)\s*[\r\n]$/ms) {
		    my ($percent, $speed, $eta) = ($1, $2, $3);
		    if (propagate_sync_callback($options, 'progress', $file, $percent, $total, $eta, $speed) eq 'canceled') {
			kill 15, $wget_pid;
			close $wget;
			return;
		    }
		    if ($_ eq "\n") {
			propagate_sync_callback($options, 'end', $file);
			($total, $file) = (undef, undef);
		    }
		}
	    } else {
		$options->{quiet} or print STDERR $buf;
	    }
	    $buf = '';
	}
    }
    $file and propagate_sync_callback($options, 'end', $file);
    chdir $cwd;
    close $wget or _error('wget');
}

sub sync_curl {
    -x "/usr/bin/curl" or die N("curl is missing\n");
    my $options = shift;
    $options = { dir => $options } if !ref $options;
    if (defined $options->{'limit-rate'} && $options->{'limit-rate'} =~ /\d$/) {
	#- use bytes by default
	$options->{'limit-rate'} .= 'B';
    }
    #- force download to be done in cachedir to avoid polluting cwd,
    #- however for curl, this is mandatory.
    (my $cwd) = getcwd() =~ /(.*)/;
    chdir($options->{dir});
    my (@ftp_files, @other_files);
    foreach (@_) {
	my ($proto, $nick, $rest) = m,^(http|ftp)://([^:/]+):(.*),,;
	if ($nick) { #- escape @ in user names
	    $nick =~ s/@/%40/;
	    $_ = "$proto://$nick:$rest";
	}
	if (m|^ftp://.*/([^/]*)$| && file_size($1) > 8192) { #- manage time stamp for large file only
	    push @ftp_files, $_;
	} else {
	    push @other_files, $_;
	}
    }
    if (@ftp_files) {
	my ($cur_ftp_file, %ftp_files_info);
	local $_;

	eval { require Date::Manip };

	#- prepare to get back size and time stamp of each file.
	my $cmd = join(" ", map { "'$_'" } "/usr/bin/curl",
	    "-q", # don't read .curlrc; some toggle options might interfer
	    ($options->{'limit-rate'} ? ("--limit-rate", $options->{'limit-rate'}) : ()),
	    ($options->{proxy} ? set_proxy({ type => "curl", proxy => $options->{proxy} }) : ()),
	    ($options->{retry} ? ('--retry', $options->{retry}) : ()),
	    "--stderr", "-", # redirect everything to stdout
	    "--disable-epsv",
	    "--connect-timeout", $CONNECT_TIMEOUT,
	    "-s", "-I",
	    "--anyauth",
	    (defined $options->{'curl-options'} ? split /\s+/, $options->{'curl-options'} : ()),
	    @ftp_files);
	$options->{debug} and $options->{debug}($cmd);
	open my $curl, "$cmd |";
	while (<$curl>) {
	    if (/Content-Length:\s*(\d+)/) {
		!$cur_ftp_file || exists($ftp_files_info{$cur_ftp_file}{size})
		    and $cur_ftp_file = shift @ftp_files;
		$ftp_files_info{$cur_ftp_file}{size} = $1;
	    }
	    if (/Last-Modified:\s*(.*)/) {
		!$cur_ftp_file || exists($ftp_files_info{$cur_ftp_file}{time})
		    and $cur_ftp_file = shift @ftp_files;
		eval {
		    $ftp_files_info{$cur_ftp_file}{time} = Date::Manip::ParseDate($1);
		};
	    }
	}
	close $curl or _error('curl');

	#- now analyse size and time stamp according to what already exists here.
	if (@ftp_files) {
	    #- re-insert back shifted element of ftp_files, because curl output above
	    #- has not been parsed correctly, so in doubt download them all.
	    push @ftp_files, keys %ftp_files_info;
	} else {
	    #- for that, it should be clear ftp_files is empty...
	    #- elsewhere, the above work was useless.
	    foreach (keys %ftp_files_info) {
		my ($lfile) = m|/([^/]*)$| or next; #- strange if we can't parse it correctly.
		my $ltime = eval { Date::Manip::ParseDate(scalar gmtime((stat $1)[9])) };
		$ltime && -s $lfile == $ftp_files_info{$_}{size} && $ftp_files_info{$_}{time} eq $ltime
		    or push @ftp_files, $_;
	    }
	}
    }
    # Indicates whether this option is available in our curl
    our $location_trusted;
    if (!defined $location_trusted) {
	$location_trusted = `/usr/bin/curl -h` =~ /location-trusted/ ? 1 : 0;
    }
    #- http files (and other files) are correctly managed by curl wrt conditional download.
    #- options for ftp files, -R (-O <file>)*
    #- options for http files, -R (-O <file>)*
    if (my @all_files = (
	    (map { ("-O", $_) } @ftp_files),
	    (map { m|/| ? ("-O", $_) : @{[]} } @other_files)))
    {
	my @l = (@ftp_files, @other_files);
	my ($buf, $file); $buf = '';
	my $cmd = join(" ", map { "'$_'" } "/usr/bin/curl",
	    "-q", # don't read .curlrc; some toggle options might interfer
	    ($options->{'limit-rate'} ? ("--limit-rate", $options->{'limit-rate'}) : ()),
	    ($options->{resume} ? ("--continue-at", "-") : ()),
	    ($options->{proxy} ? set_proxy({ type => "curl", proxy => $options->{proxy} }) : ()),
	    ($options->{retry} ? ('--retry', $options->{retry}) : ()),
	    ($options->{quiet} ? "-s" : @{[]}),
	    "-k",
	    $location_trusted ? "--location-trusted" : @{[]},
	    "-R",
	    "-f",
	    "--disable-epsv",
	    "--connect-timeout", $CONNECT_TIMEOUT,
	    "--anyauth",
	    (defined $options->{'curl-options'} ? split /\s+/, $options->{'curl-options'} : ()),
	    "--stderr", "-", # redirect everything to stdout
	    @all_files);
	$options->{debug} and $options->{debug}($cmd);
	my $curl_pid = open(my $curl, "$cmd |");
	local $/ = \1; #- read input by only one char, this is slow but very nice (and it works!).
	local $_;
	while (<$curl>) {
	    $buf .= $_;
	    if ($_ eq "\r" || $_ eq "\n") {
		if ($options->{callback}) {
		    unless (defined $file) {
			$file = shift @l;
			propagate_sync_callback($options, 'start', $file);
		    }
		    if (my ($percent, $total, $eta, $speed) = $buf =~ /^\s*(\d+)\s+(\S+)[^\r\n]*\s+(\S+)\s+(\S+)\s*[\r\n]$/ms) {
			$speed =~ s/^-//;
			if (propagate_sync_callback($options, 'progress', $file, $percent, $total, $eta, $speed) eq 'canceled') {
			    kill 15, $curl_pid;
			    close $curl;
			    die N("curl failed: download canceled\n");
			}
			#- this checks that download has actually started
			if ($_ eq "\n"
			    && !($speed == 0 && $percent == 100 && index($eta, '--') >= 0) #- work around bug 13685
			) {
			    propagate_sync_callback($options, 'end', $file);
			    $file = undef;
			}
		    } elsif ($buf =~ /^curl:/) { #- likely to be an error reported by curl
			local $/ = "\n";
			chomp $buf;
			propagate_sync_callback($options, 'error', $file, $buf);
		    }
		} else {
		    $options->{quiet} or print STDERR $buf;
		}
		$buf = '';
	    }
	}
	chdir $cwd;
	close $curl or _error('curl');
    } else {
	chdir $cwd;
    }
}

sub _calc_limit_rate {
    my $limit_rate = $_[0];
    for ($limit_rate) {
	/^(\d+)$/     and $limit_rate = int $1/1024, last;
	/^(\d+)[kK]$/ and $limit_rate = $1, last;
	/^(\d+)[mM]$/ and $limit_rate = 1024*$1, last;
	/^(\d+)[gG]$/ and $limit_rate = 1024*1024*$1, last;
    }
    $limit_rate;
}

sub sync_rsync {
    -x "/usr/bin/rsync" or die N("rsync is missing\n");
    my $options = shift;
    $options = { dir => $options } if !ref $options;
    #- force download to be done in cachedir to avoid polluting cwd.
    (my $cwd) = getcwd() =~ /(.*)/;
    chdir($options->{dir});
    my $limit_rate = _calc_limit_rate($options->{'limit-rate'});
    foreach (@_) {
	my $count = 10; #- retry count on error (if file exists).
	my $basename = basename($_);
	my $file =  m!^rsync://([^/]*::.*)! ? $1 : $_;
	propagate_sync_callback($options, 'start', $file);
	do {
	    local $_;
	    my $buf = '';
	    my $cmd = join(" ", "/usr/bin/rsync",
		($limit_rate ? "--bwlimit=$limit_rate" : @{[]}),
		($options->{quiet} ? qw(-q) : qw(--progress -v)),
		($options->{compress} ? qw(-z) : @{[]}),
		($options->{ssh} ? qq(-e $options->{ssh}) : @{[]}),
		qw(--partial --no-whole-file --no-motd --copy-links),
		(defined $options->{'rsync-options'} ? split /\s+/, $options->{'rsync-options'} : ()),
		"'$file' '$options->{dir}' 2>&1");
	    $options->{debug} and $options->{debug}($cmd);
	    open(my $rsync, "$cmd |");
	    local $/ = \1; #- read input by only one char, this is slow but very nice (and it works!).
	    local $_;
	    while (<$rsync>) {
		$buf .= $_;
		if ($_ eq "\r" || $_ eq "\n") {
		    if ($options->{callback}) {
			if (my ($percent, $speed) = $buf =~ /^\s*\d+\s+(\d+)%\s+(\S+)\s+/) {
			    propagate_sync_callback($options, 'progress', $file, $percent, undef, undef, $speed);
			} else {
			    $options->{debug} and $options->{debug}($buf);
			}
		    } else {
			$options->{quiet} or print STDERR $buf;
			$options->{debug} and $options->{debug}($buf);
		    }
		    $buf = '';
		}
	    }
	    close $rsync;
	} while ($? != 0 && --$count > 0 && -e $options->{dir} . "/$basename");
	propagate_sync_callback($options, 'end', $file);
    }
    chdir $cwd;
    $? == 0 or _error('rsync');
}

our $SSH_PATH;
sub _init_ssh_path() {
    foreach (qw(/usr/bin/ssh /bin/ssh)) {
	-x $_ and $SSH_PATH = $_;
	next;
    }
}

#- Don't generate a tmp dir name, so when we restart urpmi, the old ssh
#- connection can be reused
our $SSH_CONTROL_DIR = $ENV{TMP} || $ENV{TMPDIR} || '/tmp';
our $SSH_CONTROL_OPTION;

sub sync_ssh {
    $SSH_PATH or _init_ssh_path();
    $SSH_PATH or die N("ssh is missing\n");
    my $options = shift;
    $options = { dir => $options } if !ref $options;
    unless ($options->{'rsync-options'} =~ /(?:-e|--rsh)\b/) {
	my ($server, $user) = ('', getpwuid($<));
	$_[0] =~ /((?:\w|\.)*):/ and $server = $1;
	$_[0] =~ /((?:\w|-)*)@/ and $user = $1;
	$SSH_CONTROL_OPTION = "-o 'ControlPath $SSH_CONTROL_DIR/ssh-urpmi-$$-%h_%p_%r' -o 'ControlMaster auto'";
	if (start_ssh_master($server, $user)) {
	    $options->{ssh} = qq("$SSH_PATH $SSH_CONTROL_OPTION");
	} else {
	    #- can't start master, use single connection
	    $options->{ssh} = $SSH_PATH;
	}
    }
    sync_rsync($options, @_);
}

sub sync_prozilla {
    -x "/usr/bin/proz" or die N("prozilla is missing\n");
    my $options = shift;
    $options = { dir => $options } if !ref $options;
    #- force download to be done in cachedir to avoid polluting cwd.
    (my $cwd) = getcwd() =~ /(.*)/;
    chdir $options->{dir};
    my $proz_command = join(" ", map { "'$_'" }
	"/usr/bin/proz",
	"--no-curses",
	(defined $options->{'prozilla-options'} ? split /\s+/, $options->{'prozilla-options'} : ()),
	@_
    );
    my $ret = system($proz_command);
    chdir $cwd;
    if ($ret) {
	if ($? == -1) {
	    die N("Couldn't execute prozilla\n");
	} else {
	    _error('prozilla');
	}
    }
}

sub start_ssh_master {
    my ($server, $user) = @_;
    $server or return 0;
    if (!check_ssh_master($server, $user)) {
	system(qq($SSH_PATH -f -N $SSH_CONTROL_OPTION -M $user\@$server));
	return ! $?;
    }
    return 1;
}

sub check_ssh_master {
    my ($server, $user) = @_;
    system(qq($SSH_PATH -q -f -N $SSH_CONTROL_OPTION $user\@$server -O check));
    return ! $?;
}

END {
    #- remove ssh persistent connections
    foreach my $socket (glob "$SSH_CONTROL_DIR/ssh-urpmi-$$-*") {
	my ($server, $login) = $socket =~ /ssh-urpmi-\d+-([^_]+)_\d+_(.*)$/ or next;
	system($SSH_PATH, '-q', '-f', '-N', '-o', "ControlPath $socket", '-O', 'exit', "$login\@$server");
    }
}

#- get the width of the terminal
my $wchar = 79;
eval {
    require Term::ReadKey;
    ($wchar) = Term::ReadKey::GetTerminalSize();
    --$wchar;
};

#- default logger suitable for sync operation on STDERR only.
sub sync_logger {
    my ($mode, $file, $percent, $total, $eta, $speed) = @_;
    if ($mode eq 'start') {
	print STDERR "    $file\n";
    } elsif ($mode eq 'progress') {
	my $text;
	if (defined $total && defined $eta) {
	    $text = N("        %s%% of %s completed, ETA = %s, speed = %s", $percent, $total, $eta, $speed);
	} else {
	    $text = N("        %s%% completed, speed = %s", $percent, $speed);
	}
	if (length($text) > $wchar) { $text = substr($text, 0, $wchar) }
	print STDERR $text, " " x ($wchar - length($text)), "\r";
    } elsif ($mode eq 'end') {
	print STDERR " " x $wchar, "\r";
    } elsif ($mode eq 'error') {
	#- error is 3rd argument, saved in $percent
	print STDERR N("...retrieving failed: %s", $percent), "\n";
    }
}


sub requested_ftp_http_downloader {
    my ($urpm, $media_name) = @_;

    $urpm->{options}{downloader} || #- cmd-line switch
      $media_name && do {
	  #- per-media config
	  require urpm::media; #- help perl_checker
	  my $m = urpm::media::name2medium($urpm, $media_name);
	  $m && $m->{downloader};
      } || $urpm->{global_config}{downloader};
}

sub parse_url_with_login {
    my ($url) = @_;
    $url =~ m!([^:]*)://([^/:\@]*)(:([^/:\@]*))?\@([^/]*)(.*)! && $1 ne 'ssh' &&
      { proto => $1, login => $2, password => $4, machine => $5, dir => $6 };
}
sub url_obscuring_password {
    my ($url) = @_;
    my $u = parse_url_with_login($url);
    if ($u && $u->{password}) {
	sprintf('%s://xxx:xxx@%s%s', $u->{proto}, $u->{machine}, $u->{dir});
    } else {
	$url;
    }
}

#- $medium can be undef
#- known options: quiet, resume, callback
sub sync {
    my ($urpm, $medium, $files, %options) = @_;

    my %all_options = ( 
	dir => "$urpm->{cachedir}/partial",
	proxy => get_proxy($medium),
	$medium ? (media => $medium->{name}) : (),
	$urpm->{debug} ? (debug => $urpm->{debug}) : (),
	%options,
    );
    foreach my $cpt (qw(compress limit-rate retry wget-options curl-options rsync-options prozilla-options)) {
	$all_options{$cpt} = $urpm->{options}{$cpt} if defined $urpm->{options}{$cpt};
    }

    my $files_text = join(' ', map { url_obscuring_password($_) } @$files);
    $urpm->{debug} and $urpm->{debug}(N("retrieving %s", $files_text));

    eval { 
	_sync_webfetch_raw($urpm, $files, \%all_options); 
	$urpm->{log}(N("retrieved %s", $files_text));
	1;
    };
}

#- syncing algorithms.
sub _sync_webfetch_raw {    
    my ($urpm, $files, $options) = @_;

    my %files;
    #- currently ftp and http protocols are managed by curl or wget,
    #- ssh and rsync protocols are managed by rsync *AND* ssh.
    foreach (@$files) {
	my $proto = urpm::protocol_from_url($_) or die N("unknown protocol defined for %s", $_);
	push @{$files{$proto}}, $_;
    }
    if ($files{removable} || $files{file}) {
	my @l = map { urpm::file_from_local_url($_) } @{$files{removable} || []}, @{$files{file} || []};
	eval { sync_file($options, @l) };
	$urpm->{fatal}(10, $@) if $@;
	delete @files{qw(removable file)};
    }
    if ($files{ftp} || $files{http} || $files{https}) {
	my @available = urpm::download::available_ftp_http_downloaders();

	#- use user default downloader if provided and available
	my $requested_downloader = requested_ftp_http_downloader($urpm, $options->{media});
	my ($preferred) = grep { $_ eq $requested_downloader } @available;
	if (!$preferred) {
	    #- else first downloader of @available is the default one
	    $preferred = $available[0];
	    if ($requested_downloader && !our $webfetch_not_available) {
		$urpm->{log}(N("%s is not available, falling back on %s", $requested_downloader, $preferred));
		$webfetch_not_available = 1;
	    }
	}
	my $sync = $urpm::download::{"sync_$preferred"} or die N("no webfetch found, supported webfetch are: %s\n", join(", ", urpm::download::ftp_http_downloaders()));
	my @l = (@{$files{ftp} || []}, @{$files{http} || []}, @{$files{https} || []});
	while (@l) {
	    my $half_MAX_ARG = 131072 / 2;
	    # restrict the number of elements so that it fits on cmdline of curl/wget/proz
	    my $n = 0;
	    for (my $len = 0; $n < @l && $len < $half_MAX_ARG; $len += length($l[$n++])) {}	    
	    $sync->($options, splice(@l, 0, $n));
	}

	delete @files{qw(ftp http https)};
    }
    if ($files{rsync}) {
	sync_rsync($options, @{$files{rsync}});
	delete $files{rsync};
    }
    if ($files{ssh}) {
	my @ssh_files = map { m!^ssh://([^/]*)(.*)! ? "$1:$2" : () } @{$files{ssh}};
	sync_ssh($options, @ssh_files);
	delete $files{ssh};
    }
    %files and die N("unable to handle protocol: %s", join ', ', keys %files);
}

1;

__END__

=head1 NAME

urpm::download - download routines for the urpm* tools

=head1 SYNOPSIS

=head1 DESCRIPTION

=head1 COPYRIGHT

Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005 MandrakeSoft SA

Copyright (C) 2005, 2006 Mandriva SA

=cut
