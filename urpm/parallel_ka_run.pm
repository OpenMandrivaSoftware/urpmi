package urpm::parallel_ka_run;

use strict;

(our $VERSION) = q$Id$ =~ /(\d+\.\d+)/;
our $mput_command = 'mput';
our $rshp_command = 'rshp';

#- parallel copy
sub parallel_register_rpms {
    my ($parallel, $urpm, @files) = @_;

    $urpm->{log}("parallel_ka_run: $mput_command $parallel->{options} -- @files $urpm->{cachedir}/rpms/");
    system $mput_command, split(' ', $parallel->{options}), '--', @files, "$urpm->{cachedir}/rpms/";
    $? == 0 || $? == 256 or $urpm->{fatal}(1, urpm::N("mput failed, maybe a node is unreacheable"));

    #- keep trace of direct files.
    foreach (@files) {
	my $basename = (/^.*\/([^\/]*)$/ && $1) || $_;
	$parallel->{line} .= "'$urpm->{cachedir}/rpms/$basename' ";
    }
}

#- parallel find_packages_to_remove
sub parallel_find_remove {
    my ($parallel, $urpm, $state, $l, %options) = @_;
    my ($test, $node, %bad_nodes, %base_to_remove, %notfound);
    local $_;

    #- keep in mind if the previous selection is still active, it avoid
    #- to re-start urpme --test on each node.
    if ($options{find_packages_to_remove}) {
	delete $state->{rejected};
	delete $urpm->{error_remove};
	$test = '--test ';
    } else {
	@{$urpm->{error_remove} || []} and return @{$urpm->{error_remove}};
	#- no need to restart what has been started before.
	$options{test} and return keys %{$state->{rejected}};
	$test = '--force ';
    }

    #- now try an iteration of urpmq.
    $urpm->{log}("parallel_ka_run: $rshp_command -v $parallel->{options} -- urpme --no-locales --auto $test" . (join ' ', map { "'$_'" } @$l));
    open my $fh, "$rshp_command -v $parallel->{options} -- urpme --no-locales --auto $test" . join(' ', map { "'$_'" } @$l) . " 2>&1 |";
    while (<$fh>) {
	chomp;
	s/<([^>]*)>.*:->:(.*)/$2/ and $node = $1;
	/^\s*$/ and next;
	/Checking to remove the following packages/ and next;
	/To satisfy dependencies, the following packages are going to be removed/
	    and $urpm->{fatal}(1, urpm::N("node %s has an old version of urpme, please upgrade", $node));
	if (/unknown packages?:? (.*)/) {
	    #- keep in mind unknown package from the node, because it should not be a fatal error
	    #- if other node have it.
	    @notfound{split ", ", $1} = ();
	} elsif (/The following packages contain ([^:]*): (.*)/) {
	    $options{callback_fuzzy} && $options{callback_fuzzy}->($urpm, $1, split " ", $2)
	      or delete $state->{rejected}, last;
	} elsif (/removing package (.*) will break your system/) {
	    $base_to_remove{$1} = undef;
	} elsif (/removing \S/) {
	    #- this is log for newer urpme, so do not try to remove removing...
	} elsif (/Remov(?:al|ing) failed/) {
	    $bad_nodes{$node} = [];
	} else {
	    if (exists $bad_nodes{$node}) {
		/^\s+(.+)/ and push @{$bad_nodes{$node}}, $1;
	    } else {
		s/\s*\(.*//; #- remove reason (too complex to handle and needed to be removed).
		$state->{rejected}{$_}{removed} = 1;
		$state->{rejected}{$_}{nodes}{$node} = undef;
	    }
	}
    }
    close $fh or $urpm->{fatal}(1, urpm::N("rshp failed, maybe a node is unreacheable"));

    #- check base, which has been delayed until there.
    $options{callback_base} and %base_to_remove and $options{callback_base}->($urpm, keys %base_to_remove)
      || return ();

    #- build error list contains all the error returned by each node.
    $urpm->{error_remove} = [];
    foreach (keys %bad_nodes) {
	my $msg = urpm::N("on node %s", $_);
	foreach (@{$bad_nodes{$_}}) {
	    push @{$urpm->{error_remove}}, "$msg, $_";
	}
    }

    #- if at least one node has the package, it should be seen as unknown...
    delete @notfound{map { /^(.*)-[^-]*-[^-]*$/ } keys %{$state->{rejected}}};
    if (%notfound) {
	$options{callback_notfound} && $options{callback_notfound}->($urpm, keys %notfound)
	  or delete $state->{rejected};
    }

    keys %{$state->{rejected}};
}

#- parallel resolve_dependencies
sub parallel_resolve_dependencies {
    my ($parallel, $synthesis, $urpm, $state, $requested, %options) = @_;
    my (%avoided, %requested);

    #- first propagate the synthesis file to all machine.
    $urpm->{ui_msg}("parallel_ka_run: $mput_command $parallel->{options} -- '$synthesis' '$synthesis'", urpm::N("Propagating synthesis to nodes..."));
    system "$mput_command $parallel->{options} -- '$synthesis' '$synthesis'";
    $? == 0 || $? == 256 or $urpm->{fatal}(1, urpm::N("mput failed, maybe a node is unreacheable"));
    $parallel->{synthesis} = $synthesis;

    #- compute command line of urpm? tools.
    my $line = $parallel->{line} . ($options{auto_select} ? ' --auto-select' : '') . ($options{keep} ? ' --keep' : '');
    foreach (keys %$requested) {
	if (/\|/) {
	    #- taken from URPM::Resolve to filter out choices, not complete though.
	    my $packages = $urpm->find_candidate_packages($_);
	    foreach (values %$packages) {
		my ($best_requested, $best);
		foreach (@$_) {
		    exists $state->{selected}{$_->id} and $best_requested = $_, last;
		    exists $avoided{$_->name} and next;
		    if ($best_requested || exists $requested{$_->id}) {
			if ($best_requested && $best_requested != $_) {
			    $_->compare_pkg($best_requested) > 0 and $best_requested = $_;
			} else {
			    $best_requested = $_;
			}
		    } elsif ($best && $best != $_) {
			$_->compare_pkg($best) > 0 and $best = $_;
		    } else {
			$best = $_;
		    }
		}
		$_ = $best_requested || $best;
	    }
	    #- simplified choices resolution.
	    my $choice = $options{callback_choices}->($urpm, undef, $state, [ values %$packages ]);
	    if ($choice) {
		$urpm->{source}{$choice->id} and next; #- local packages have already been added.
		$line .= ' ' . $choice->fullname;
	    }
	} else {
	    my $pkg = $urpm->{depslist}[$_] or next;
	    $urpm->{source}{$pkg->id} and next; #- local packages have already been added.
	    $line .= ' ' . $pkg->fullname;
	}
    }

    #- execute urpmq to determine packages to install.
    my ($node, $cont, %chosen);
    local $_;
    do {
	$cont = 0; #- prepare to stop iteration.
	#- the following state should be cleaned for each iteration.
	delete $state->{selected};
	#- now try an iteration of urpmq.
	$urpm->{ui_msg}("parallel_ka_run: $rshp_command -v $parallel->{options} -- urpmq --synthesis $synthesis -fduc $line " . join(' ', keys %chosen), urpm::N("Resolving dependencies on nodes..."));
	open my $fh, "$rshp_command -v $parallel->{options} -- urpmq --synthesis $synthesis -fduc $line " . join(' ', keys %chosen) . " |";
	while (<$fh>) {
	    chomp;
	    s/<([^>]*)>.*:->:(.*)/$2/ and $node = $1;
	    if (my ($action, $what) = /^\@([^\@]*)\@(.*)/) {
		if ($action eq 'removing') {
		    $state->{rejected}{$what}{removed} = 1;
		    $state->{rejected}{$what}{nodes}{$node} = undef;
		}
	    } elsif (/\|/) {
		#- distant urpmq returned a choices, check if it has already been chosen
		#- or continue iteration to make sure no more choices are left.
		$cont ||= 1; #- invalid transitory state (still choices is strange here if next sentence is not executed).
		unless (grep { exists $chosen{$_} } split '\|', $_) {
		    my $choice = $options{callback_choices}->($urpm, undef, $state, [ map { $urpm->search($_) } split '\|', $_ ]);
		    if ($choice) {
			$chosen{scalar $choice->fullname} = $choice;
			#- it has not yet been chosen so need to ask user.
			$cont = 2;
		    } else {
			#- no choices resolved, so forget it (no choices means no choices at all).
			$cont = 0;
		    }
		}
	    } else {
		my $pkg = $urpm->search($_) or next;
		$state->{selected}{$pkg->id}{$node} = $_;
	    }
	}
	close $fh or $urpm->{fatal}(1, urpm::N("rshp failed, maybe a node is unreacheable"));
	#- check for internal error of resolution.
	$cont == 1 and die "internal distant urpmq error on choice not taken";
    } while $cont;

    #- keep trace of what has been chosen finally (if any).
    $parallel->{line} = "$line ".join(' ', keys %chosen);
}

#- parallel install.
sub parallel_install {
    my ($parallel, $urpm, undef, $install, $upgrade, %options) = @_;

    $urpm->{ui_msg}("parallel_ka_run: $mput_command $parallel->{options} -- ".join(' ', values %$install, values %$upgrade)." $urpm->{cachedir}/rpms/", urpm::N("Distributing files to nodes..."));
    system $mput_command, split(' ', $parallel->{options}), '--', values %$install, values %$upgrade, "$urpm->{cachedir}/rpms/";
    $? == 0 || $? == 256 or $urpm->{fatal}(1, urpm::N("mput failed, maybe a node is unreacheable"));

    local $_;
    my ($node, %bad_nodes);
    $urpm->{ui_msg}("parallel_ka_run: $rshp_command -v $parallel->{options} -- urpmi --pre-clean --no-locales --test --no-verify-rpm --auto --synthesis $parallel->{synthesis} $parallel->{line}",
	urpm::N("Verifying if install is possible on nodes..."));
    open my $fh, "$rshp_command -v $parallel->{options} -- urpmi --pre-clean --no-locales --test --no-verify-rpm --auto --synthesis $parallel->{synthesis} $parallel->{line} |";
    while (<$fh>) {
	chomp;
	s/<([^>]*)>.*:->:(.*)/$2/ and $node = $1;
	/^\s*$/ and next;
	$bad_nodes{$node} .= $_;
	/Installation failed/ and $bad_nodes{$node} = '';
	/Installation is possible/ and delete $bad_nodes{$node};
    }
    close $fh or $urpm->{fatal}(1, urpm::N("rshp failed, maybe a node is unreacheable"));

    foreach (keys %{$parallel->{nodes}}) {
	exists $bad_nodes{$_} or next;
	$urpm->{error}(urpm::N("Installation failed on node %s", $_) . ":\n" . $bad_nodes{$_});
    }
    %bad_nodes and return;

    if ($options{test}) {
	$urpm->{error}(urpm::N("Installation is possible"));
	1;
    } else {
	my $line = $parallel->{line} . ($options{excludepath} ? " --excludepath '$options{excludepath}'" : "");
	#- continue installation.
        $urpm->{ui_msg}("parallel_ka_run: $rshp_command $parallel->{options} -- urpmi --no-locales --no-verify-rpm --auto --synthesis $parallel->{synthesis} $line", urpm::N("Installing packages on nodes..."));
	system("$rshp_command $parallel->{options} -- urpmi --no-locales --no-verify-rpm --auto --synthesis $parallel->{synthesis} $line") == 0;
    }
}

#- allow to bootstrap from urpmi code directly (namespace is urpm).

package urpm;

sub handle_parallel_options {
    my (undef, $options) = @_;
    my ($media, $ka_run_options) = $options =~ /ka-run(?:\(([^\)]*)\))?:(.*)/;
    if ($ka_run_options) {
	my ($flush_nodes, %nodes);
	foreach (split ' ', $ka_run_options) {
	    if ($_ eq '-m') {
		$flush_nodes = 1;
	    } else {
		$flush_nodes and $nodes{/host=([^,]*)/ ? $1 : $_} = undef;
		undef $flush_nodes;
	    }
	}
	return bless {
	    media   => $media,
	    options => $ka_run_options,
	    nodes   => \%nodes,
	}, "urpm::parallel_ka_run";
    }
    return undef;
}

1;
