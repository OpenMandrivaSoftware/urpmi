package urpm::parallel_ssh;

use Time::HiRes qw(gettimeofday);

sub _localhost { $_[0] eq 'localhost' }
sub _nolock    { &_localhost ? '--nolock ' : '' }
sub _ssh       { &_localhost ? '' : "ssh $_[0] " }
sub _host      { &_localhost ? '' : "$_[0]:" }

#- parallel copy
sub parallel_register_rpms {
    my ($parallel, $urpm, @files) = @_;

    foreach (keys %{$parallel->{nodes}}) {
	$urpm->{log}("parallel_ssh: scp @files $_:$urpm->{cachedir}/rpms");
	if (_localhost($_)) {
	    my @f = grep { ! m!^$urpm->{cachedir}/rpms! } @files;
	    @f and system 'cp' => @f, "$urpm->{cachedir}/rpms";
	} else {
	    system 'scp' => @files, "$_:$urpm->{cachedir}/rpms";
	}
	$? == 0 or $urpm->{fatal}(1, urpm::N("scp failed on host %s (%d)", $_, $? >> 8));
    }

    #- keep trace of direct files.
    foreach (@files) {
	my $basename = (/^.*\/([^\/]*)$/ && $1) || $_;
	$parallel->{line} .= "'$urpm->{cachedir}/rpms/$basename' ";
    }
}

#- parallel find_packages_to_remove
sub parallel_find_remove {
    my ($parallel, $urpm, $state, $l, %options) = @_;
    my ($test, %bad_nodes, %base_to_remove, %notfound);
    local $_;

    #- keep in mind if the previous selection is still active, it avoids
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

    #- now try an iteration of urpme.
    foreach my $node (keys %{$parallel->{nodes}}) {
	my $command = _ssh($node) . "urpme --no-locales --auto $test" . (join ' ', map { "'$_'" } @$l);
        $urpm->{log}("parallel_ssh: $command");
	open my $fh, "$command 2>&1 |"
	    or $urpm->{fatal}(1, "Can't fork ssh: $!");
	while (defined ($_ = <$fh>)) {
	    chomp;
	    /^\s*$/ and next;
	    /Checking to remove the following packages/ and next;
	    /To satisfy dependencies, the following packages are going to be removed/
	      and $urpm->{fatal}(1, ("node %s has bad version of urpme, please upgrade", $node));
	    if (/unknown packages?:? (.*)/) {
		#- remember unknown packages from the node, because it should not be a fatal error
		#- if other nodes have it.
		@notfound{split /, /, $1} = ();
	    } elsif (/The following packages contain ([^:]*): (.*)/) {
		$options{callback_fuzzy} and $options{callback_fuzzy}->($urpm, $1, split " ", $2)
		  or delete $state->{rejected}, last;
	    } elsif (/removing package (.*) will break your system/) {
		$base_to_remove{$1} = undef;
	    } elsif (/removing \S/) {
		#- this is log for newer urpme, so do not try to remove removing...
	    } elsif (/Removing failed/) {
		$bad_nodes{$node} = [];
	    } else {
		if (exists $bad_nodes{$node}) {
		    /^\s+(.*)/ and push @{$bad_nodes{$node}}, $1;
		} else {
		    s/\s*\(.*//; #- remove reason (too complex to handle, needs to be removed)
		    $state->{rejected}{$_}{removed} = 1;
		    $state->{rejected}{$_}{nodes}{$node} = undef;
		}
	    }
	}
	close $fh;
    }

    #- check base, which has been delayed until there.
    $options{callback_base} and keys %base_to_remove
	and $options{callback_base}->($urpm, keys %base_to_remove) || return ();

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
    if (keys %notfound) {
	$options{callback_notfound} and $options{callback_notfound}->($urpm, keys %notfound)
	  or delete $state->{rejected};
    }

    keys %{$state->{rejected}};
}

#- parallel resolve_dependencies
sub parallel_resolve_dependencies {
    my ($parallel, $synthesis, $urpm, $state, $requested, %options) = @_;

    #- first propagate the synthesis file to all machines
    foreach (grep { !_localhost($_) } keys %{$parallel->{nodes}}) {
	$urpm->{ui_msg}("parallel_ssh: scp -q $synthesis $_:$synthesis", urpm::N("Propagating synthesis to %s...", $_));
	system "scp", "-q", $synthesis, _host($_) . $synthesis;
	$? == 0 or $urpm->{fatal}(1, urpm::N("scp failed on host %s (%d)", $_, $? >> 8));
    }
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
		    if ($best_requested) {
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
	    #- simplified choice resolution.
	    my $choice = $options{callback_choices}->($urpm, undef, $state, [ values %$packages ]);
	    if ($choice) {
		$urpm->{source}{$choice->id} and next; #- local packages have already been added.
		$line .= ' ' . $choice->fullname;
	    }
	} else {
	    my $pkg = $urpm->{depslist}[$_] or next;
	    $urpm->{source}{$pkg->id} and next; #- local packages have already been added.
	    $line .= ' '.$pkg->fullname;
	}
    }

    #- execute urpmq to determine packages to install.
    my ($cont, %chosen);
    local $_;
    do {
	$cont = 0; #- prepare to stop iteration.
	#- the following state should be cleaned for each iteration.
	delete $state->{selected};
	#- now try an iteration of urpmq.
	foreach my $node (keys %{$parallel->{nodes}}) {
	    my $command = _ssh($node) . "urpmq " . _nolock($node) . "--synthesis $synthesis -fduc $line " . join(' ', keys %chosen);
	    $urpm->{ui_msg}("parallel_ssh: $command", urpm::N("Resolving dependencies on %s...", $node));
	    open my $fh, "$command |"
		or $urpm->{fatal}(1, "Can't fork ssh: $!");
	    while (defined ($_ = <$fh>)) {
		chomp;
		if (my ($action, $what) = /^\@([^\@]*)\@(.*)/) {
		    if ($action eq 'removing') {
			$state->{rejected}{$what}{removed} = 1;
			$state->{rejected}{$what}{nodes}{$node} = undef;
		    }
		} elsif (/\|/) {
		    #- distant urpmq returned a choices, check if it has already been chosen
		    #- or continue iteration to make sure no more choices are left.
		    $cont ||= 1; #- invalid transitory state (still choices is strange here if next sentence is not executed).
		    unless (grep { exists $chosen{$_} } split /\|/, $_) {
			my $choice = $options{callback_choices}->($urpm, undef, $state, [ map { $urpm->search($_) } split /\|/, $_ ]);
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
		    my $pkg = $urpm->search($_) or next; #TODO
		    $state->{selected}{$pkg->id}{$node} = $_;
		}
	    }
	    close $fh or $urpm->{fatal}(1, urpm::N("host %s does not have a good version of urpmi (%d)", $node, $? >> 8));
	}
	#- check for internal error of resolution.
	$cont == 1 and die "internal distant urpmq error on choice not taken";
    } while $cont;

    #- keep trace of what has been chosen finally (if any).
    $parallel->{line} = "$line " . join(' ', keys %chosen);
}

#- parallel install.
sub parallel_install {
    my ($parallel, $urpm, undef, $install, $upgrade, %options) = @_;

    foreach (keys %{$parallel->{nodes}}) {
	my @sources = (values %$install, values %$upgrade);
	$urpm->{ui_msg}("parallel_ssh: scp @sources $_:$urpm->{cachedir}/rpms", urpm::N("Distributing files to %s...", $_));
	if (_localhost($_)) {
	    my @f = grep { ! m!^$urpm->{cachedir}/rpms! } @sources;
	    @f and system 'cp' => @f, "$urpm->{cachedir}/rpms";
	} else {
	    system 'scp' => @sources, "$_:$urpm->{cachedir}/rpms";
	}
	$? == 0 or $urpm->{fatal}(1, urpm::N("scp failed on host %s (%d)", $_, $? >> 8));
    }

    my %bad_nodes;
    foreach my $node (keys %{$parallel->{nodes}}) {
	local $_;
	my $command = _ssh($node) . "urpmi --pre-clean --no-locales --test --no-verify-rpm --auto " . _nolock($node) . "--synthesis $parallel->{synthesis} $parallel->{line}";
	$urpm->{ui_msg}("parallel_ssh: $command", urpm::N("Verifying if install is possible on %s...", $node));
	open my $fh, "$command |"
	    or $urpm->{fatal}(1, "Can't fork ssh: $!");
	while ($_ = <$fh>) {
	    $bad_nodes{$node} .= $_;
	    /Installation failed/ and $bad_nodes{$node} = '';
	    /Installation is possible/ and delete $bad_nodes{$node}, last;
	}
	close $fh;
    }
    foreach (keys %{$parallel->{nodes}}) {
	exists $bad_nodes{$_} or next;
	$urpm->{error}(urpm::N("Installation failed on node %s", $_) . ":\n" . $bad_nodes{$_});
    }
    keys %bad_nodes and return;

    if ($options{test}) {
	$urpm->{error}(urpm::N("Installation is possible"));
	1;
    } else {
	my $line = $parallel->{line} . ($options{excludepath} ? " --excludepath $options{excludepath}" : "");
	#- continue installation on each node
	foreach my $node (keys %{$parallel->{nodes}}) {
	    my $command = _ssh($node) . "urpmi --no-locales --no-verify-rpm --auto " . _nolock($node) . "--synthesis $parallel->{synthesis} $line";
	    $urpm->{ui_msg}("parallel_ssh: $command", urpm::N("Performing install on %s...", $node));
	    $urpm->{ui}{progress}->(0) if ref $urpm->{ui}{progress};
	    open my $fh, "$command |"
		or $urpm->{fatal}(1, "Can't fork ssh: $!");
            local $/ = \1;
            my $log;
            my $last_time;
            while ($_ = <$fh>) {
                print;
                $log .= $_;
                /\n/ and $log = '';
                if (my ($msg, $progress) = $log =~ /^\s*(\S+)\s+(#+)/) {
                    if ($urpm->{ui} && (gettimeofday() - $last_time > 0.15 || length($progress) == 45)) {
                        $urpm->{ui_msg}->($msg =~ /\d+:(\S+)/ ? urpm::N("Installing %s on %s...", $1, $node)
                                                               : urpm::N("Preparing install on %s...", $node));
                        $urpm->{ui}{progress}->(length($progress)/45) if ref $urpm->{ui}{progress};
                        $last_time = gettimeofday();
                    }
                }
            }
            close $fh;
	}
    }
}

#- allow bootstrap from urpmi code directly (namespace is urpm).
package urpm;
sub handle_parallel_options {
    my (undef, $options) = @_;
    my ($id, @nodes) = split /:/, $options;

    if ($id =~ /^ssh(?:\(([^\)]*)\))?$/) {
	my %nodes; @nodes{@nodes} = undef;
	return bless {
	    media   => $1,
	    nodes   => \%nodes,
	}, "urpm::parallel_ssh";
    }

    return undef;
}

1;
