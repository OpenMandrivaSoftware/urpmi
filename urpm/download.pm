package urpm::download;

use strict;
use urpm::msg;
use urpm::cfg;
use Cwd;

sub import () {
    my $c = caller;
    no strict 'refs';
    foreach my $symbol (qw(get_proxy set_proxy
	propagate_sync_callback
	sync_file sync_wget sync_curl sync_rsync sync_ssh
    )) {
	*{$c.'::'.$symbol} = *$symbol;
    }
}

sub get_proxy () {
    my $proxy = {
	http_proxy => undef ,
	ftp_proxy => undef ,
	user => undef,
	pwd => undef
    };
    local $_;
    open my $f, $urpm::cfg::PROXY_CFG or return undef;
    while (<$f>) {
	chomp; s/#.*$//; s/^\s*//; s/\s*$//;
	/^http_proxy\s*=\s*(.*)$/ and $proxy->{http_proxy} = $1, next;
	/^ftp_proxy\s*=\s*(.*)$/ and $proxy->{ftp_proxy} = $1, next;
	/^proxy_user\s*=\s*(.*):(.*)$/ and do {
	    $proxy->{user} = $1;
	    $proxy->{pwd} = $2;
	    next;
	};
	next;
    }
    close $f;
    bless $proxy;
}

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

sub propagate_sync_callback {
    my $options = shift @_;
    if (ref($options) && $options->{callback}) {
	my $mode = shift @_;
	if ($mode =~ /^(?:start|progress|end)$/) {
	    my $file = shift @_;
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
	my ($in) = m!^(?:removable[^:]*|file):/(.*)!;
	propagate_sync_callback($options, 'start', $_);
	system("cp", "-p", "-R", $in || $_, ref($options) ? $options->{dir} : $options) and
	  die N("copy failed: %s", $@);
	propagate_sync_callback($options, 'end', $_);
    }
}

sub sync_wget {
    -x "/usr/bin/wget" or die N("wget is missing\n");
    my $options = shift @_;
    $options = { dir => $options } if !ref $options;
    #- force download to be done in cachedir to avoid polluting cwd.
    my $cwd = getcwd();
    chdir $options->{dir};
    my ($buf, $total, $file) = ('', undef, undef);
    my $wget_pid = open my $wget, join(" ", map { "'$_'" }
	#- construction of the wget command-line
	"/usr/bin/wget",
	($options->{limit_rate} ? "--limit-rate=$options->{limit_rate}" : ()),
	($options->{resume} ? "--continue" : ()),
	($options->{proxy} ? set_proxy({ type => "wget", proxy => $options->{proxy} }) : ()),
	($options->{callback} ? ("--progress=bar:force", "-o", "-") :
	    $options->{quiet} ? "-q" : @{[]}),
	"--retr-symlinks",
	"-NP",
	$options->{dir},
	@_
    ) . " |";
    local $/ = \1; #- read input by only one char, this is slow but very nice (and it works!).
    while (<$wget>) {
	$buf .= $_;
	if ($_ eq "\r" || $_ eq "\n") {
	    if ($options->{callback}) {
		if ($buf =~ /^--\d\d:\d\d:\d\d--\s+(\S.*)\n/ms) {
		    $file && $file ne $1 and propagate_sync_callback($options, 'end', $file);
		    ! defined $file and propagate_sync_callback($options, 'start', $file = $1);
		} elsif (defined $file && ! defined $total && $buf =~ /==>\s+RETR/) {
		    $total = '';
		} elsif (defined $total && $total eq '' && $buf =~ /^[^:]*:\s+(\d\S*)/) {
		    $total = $1;
		} elsif (my ($percent, $speed, $eta) = $buf =~ /^\s*(\d+)%.*\s+(\S+)\s+ETA\s+(\S+)\s*[\r\n]$/ms) {
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
    close $wget or die N("wget failed: exited with %d or signal %d\n", $? >> 8, $? & 127);
}

sub sync_curl {
    -x "/usr/bin/curl" or die N("curl is missing\n");
    my $options = shift @_;
    $options = { dir => $options } if !ref $options;
    #- force download to be done in cachedir to avoid polluting cwd,
    #- however for curl, this is mandatory.
    my $cwd = getcwd();
    chdir($options->{dir});
    my (@ftp_files, @other_files);
    foreach (@_) {
	m|^ftp://.*/([^/]*)$| && -e $1 && -s _ > 8192 and do {
	    push @ftp_files, $_; next;
	}; #- manage time stamp for large file only.
	push @other_files, $_;
    }
    if (@ftp_files) {
	my ($cur_ftp_file, %ftp_files_info);

	eval { require Date::Manip };

	#- prepare to get back size and time stamp of each file.
	open my $curl, join(" ", map { "'$_'" } "/usr/bin/curl",
	    ($options->{limit_rate} ? ("--limit-rate", $options->{limit_rate}) : ()),
	    ($options->{proxy} ? set_proxy({ type => "curl", proxy => $options->{proxy} }) : ()),
	    "--stderr", "-", "-s", "-I", @ftp_files) . " |";
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
		    #- remove day and hour.
		    $ftp_files_info{$cur_ftp_file}{time} =~ s/(\d{6}).{4}(.*)/$1$2/;
		};
	    }
	}
	close $curl;

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
		$ltime =~ s/(\d{6}).{4}(.*)/$1$2/; #- remove day and hour.
		-s $lfile == $ftp_files_info{$_}{size} && $ftp_files_info{$_}{time} eq $ltime or
		push @ftp_files, $_;
	    }
	}
    }
    #- http files (and other files) are correctly managed by curl to conditionnal download.
    #- options for ftp files, -R (-O <file>)*
    #- options for http files, -R (-z file -O <file>)*
    if (my @all_files = (
	    (map { ("-O", $_) } @ftp_files),
	    (map { m|/([^/]*)$| ? ("-z", $1, "-O", $_) : @{[]} } @other_files)))
    {
	my @l = (@ftp_files, @other_files);
	my ($buf, $file) = ('');
	my $curl_pid = open my $curl, join(" ", map { "'$_'" } "/usr/bin/curl",
	    ($options->{limit_rate} ? ("--limit-rate", $options->{limit_rate}) : ()),
	    ($options->{resume} ? ("--continue-at", "-") : ()),
	    ($options->{proxy} ? set_proxy({ type => "curl", proxy => $options->{proxy} }) : ()),
	    ($options->{quiet} && !$options->{verbose} ? "-s" : @{[]}),
	    "-k",
	    `curl -h` =~ /location-trusted/ ? "--location-trusted" : @{[]},
	    "-R", "-f", "--stderr", "-",
	    @all_files) . " |";
	local $/ = \1; #- read input by only one char, this is slow but very nice (and it works!).
	while (<$curl>) {
	    $buf .= $_;
	    if ($_ eq "\r" || $_ eq "\n") {
		if ($options->{callback}) {
		    unless (defined $file) {
			$file = shift @l;
			propagate_sync_callback($options, 'start', $file);
		    }
		    if (my ($percent, $total, $eta, $speed) = $buf =~ /^\s*(\d+)\s+(\S+)[^\r\n]*\s+(\S+)\s+(\S+)[\r\n]$/ms) {
			if (propagate_sync_callback($options, 'progress', $file, $percent, $total, $eta, $speed) eq 'canceled') {
			    kill 15, $curl_pid;
			    close $curl;
			    return;
			}
			if ($_ eq "\n") {
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
	close $curl or die N("curl failed: exited with %d or signal %d\n", $? >> 8, $? & 127);
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
    my $options = shift @_;
    $options = { dir => $options } if !ref $options;
    #- force download to be done in cachedir to avoid polluting cwd.
    my $cwd = getcwd();
    chdir($options->{dir});
    my $limit_rate = _calc_limit_rate $options->{limit_rate};
    foreach (@_) {
	my $count = 10; #- retry count on error (if file exists).
	my $basename = basename($_);
	my ($file) = m|^rsync://(.*)| or next;
	$file =~ /::/ or $file = $_;
	propagate_sync_callback($options, 'start', $file);
	do {
	    local $_;
	    my $buf = '';
	    open my $rsync, join(" ", map { "'$_'" } "/usr/bin/rsync",
		($limit_rate ? "--bwlimit=$limit_rate" : ()),
		($options->{quiet} ? qw(-q) : qw(--progress -v)),
		if_($options->{compress}, qw(-z)),
		qw(--partial --no-whole-file),
		$file, $options->{dir}) . " |";
	    local $/ = \1; #- read input by only one char, this is slow but very nice (and it works!).
	    while (<$rsync>) {
		$buf .= $_;
		if ($_ eq "\r" || $_ eq "\n") {
		    if ($options->{callback}) {
			if (my ($percent, $speed) = $buf =~ /^\s*\d+\s+(\d+)%\s+(\S+)\s+/) {
			    propagate_sync_callback($options, 'progress', $file, $percent, undef, undef, $speed);
			}
		    } else {
			$options->{quiet} or print STDERR $buf;
		    }
		    $buf = '';
		}
	    }
	    close $rsync;
	} while ($? != 0 && --$count > 0 && -e $options->{dir} . "/$basename");
	propagate_sync_callback($options, 'end', $file);
    }
    chdir $cwd;
    $? == 0 or die N("rsync failed: exited with %d or signal %d\n", $? >> 8, $? & 127);
}

sub sync_ssh {
    -x "/usr/bin/rsync" or die N("rsync is missing\n");
    -x "/usr/bin/ssh" or die N("ssh is missing\n");
    my $options = shift @_;
    $options = { dir => $options } if !ref $options;
    #- force download to be done in cachedir to avoid polluting cwd.
    my $cwd = getcwd();
    chdir($options->{dir});
    my $limit_rate = _calc_limit_rate $options->{limit_rate};
    foreach my $file (@_) {
	my $count = 10; #- retry count on error (if file exists).
	my $basename = basename($file);
	propagate_sync_callback($options, 'start', $file);
	do {
	    local $_;
	    my $buf = '';
	    open my $rsync, join(" ", map { "'$_'" } "/usr/bin/rsync",
		($limit_rate ? "--bwlimit=$limit_rate" : ()),
		($options->{quiet} ? qw(-q) : qw(--progress -v)),
		if_($options->{compress}, qw(-z)),
		qw(--partial -e ssh), $file, $options->{dir}) . " |";
	    local $/ = \1; #- read input by only one char, this is slow but very nice (and it works!).
	    while (<$rsync>) {
		$buf .= $_;
		if ($_ eq "\r" || $_ eq "\n") {
		    if ($options->{callback}) {
			if (my ($percent, $speed) = $buf =~ /^\s*\d+\s+(\d+)%\s+(\S+)\s+/) {
			    propagate_sync_callback($options, 'progress', $file, $percent, undef, undef, $speed);
			}
		    } else {
			$options->{quiet} or print STDERR $buf;
		    }
		    $buf = '';
		}
	    }
	    close $rsync;
	} while ($? != 0 && --$count > 0 && -e $options->{dir} . "/$basename");
	propagate_sync_callback($options, 'end', $file);
    }
    chdir $cwd;
    $? == 0 or die N("rsync failed: exited with %d or signal %d\n", $? >> 8, $? & 127);
}

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
	print STDERR $text, " " x (79 - length($text)), "\r";
    } elsif ($mode eq 'end') {
	print STDERR " " x 79, "\r";
    } elsif ($mode eq 'error') {
	#- error is 3rd argument, saved in $percent
	print STDERR N("...retrieving failed: %s", $percent), "\n";
    }
}

1;

__END__

=head1 NAME

urpm::download - download routines for the urpm* tools

=head1 SYNOPSIS

=head1 DESCRIPTION

=cut
