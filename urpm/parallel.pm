package urpm::parallel; # $Id$

use urpm;
use urpm::util;
use urpm::msg;


sub configure {
    my ($urpm, $alias) = @_;
    my @parallel_options;
    #- read parallel configuration
    foreach (cat_("/etc/urpmi/parallel.cfg")) {
	chomp; s/#.*$//; s/^\s*//; s/\s*$//;
	/\s*([^:]*):(.*)/ or $urpm->{error}(N("unable to parse \"%s\" in file [%s]", $_, "/etc/urpmi/parallel.cfg")), next;
	$1 eq $alias and push @parallel_options, $2;
    }
    #- if a configuration option has been found, use it; else fatal error.
    my $parallel_handler;
    if (@parallel_options) {
	foreach my $dir (grep { -d $_ } map { "$_/urpm" } @INC) {
	    foreach my $pm (grep { -f $_ } glob("$dir/parallel_*.pm")) {
		#- load parallel modules
		$urpm->{log}->(N("examining parallel handler in file [%s]", $pm));
		# perl_checker: require urpm::parallel_ka_run
		# perl_checker: require urpm::parallel_ssh
		eval { require $pm; $parallel_handler = $urpm->handle_parallel_options(join("\n", @parallel_options)) };
		$parallel_handler and last;
	    }
	    $parallel_handler and last;
	}
    }
    if ($parallel_handler) {
	if ($parallel_handler->{nodes}) {
	    $urpm->{log}->(N("found parallel handler for nodes: %s", join(', ', keys %{$parallel_handler->{nodes}})));
	}
	$urpm->{parallel_handler} = $parallel_handler;
    } else {
	$urpm->{fatal}(1, N("unable to use parallel option \"%s\"", $alias));
    }
}

sub resolve_dependencies {
    my ($urpm, $state, $requested, %options) = @_;

    #- build the global synthesis file first.
    my $file = "$urpm->{cachedir}/partial/parallel.cz";
    unlink $file;
    foreach (@{$urpm->{media}}) {
	urpm::media::is_valid_medium($_) or next;
	my $f = urpm::media::any_synthesis($urpm, $_);
	system "cat '$f' >> '$file'";
    }
    #- let each node determine what is requested, according to handler given.
    $urpm->{parallel_handler}->parallel_resolve_dependencies($file, $urpm, $state, $requested, %options);
}

#- remove packages from node as remembered according to resolving done.
sub remove {
    my ($urpm, $remove, %options) = @_;
    my $state = {};
    my $callback = sub { $urpm->{fatal}(1, "internal distributed remove fatal error") };
    $urpm->{parallel_handler}->parallel_find_remove($urpm, $state, $remove, %options,
						    callback_notfound => undef,
						    callback_fuzzy => $callback,
						    callback_base => $callback,
						   );
}

#- parallel find_packages_to_remove
sub parallel_find_remove {
    my ($parallel, $urpm, $state, $l, %options) = @_;

    my ($test, $pkgs) = _find_remove_pre($urpm, $state, %options);
    $pkgs and return @$pkgs;

    my (%bad_nodes, %base_to_remove, %notfound);

    #- now try an iteration of urpme.
    $parallel->urpm_popen($urpm, 'urpme', "--auto $test" . join(' ', map { "'$_'" } @$l) . ' 2>&1', sub {
	my ($node, $s) = @_;

	_parse_urpme_output($urpm, $state, $node, $s, 
					   \%notfound, \%base_to_remove, \%bad_nodes, %options);
    });

    #- check base, which has been delayed until there.
    if ($options{callback_base} && %base_to_remove) {
	$options{callback_base}->($urpm, keys %base_to_remove) or return ();
    }

    #- build error list contains all the error returned by each node.
    $urpm->{error_remove} = [ map {
	my $msg = N("on node %s", $_);
	map { "$msg, $_" } @{$bad_nodes{$_}};
    } keys %bad_nodes ];

    #- if at least one node has the package, it should be seen as unknown...
    delete @notfound{map { /^(.*)-[^-]*-[^-]*$/ } keys %{$state->{rejected}}};
    if (%notfound) {
	$options{callback_notfound} && $options{callback_notfound}->($urpm, keys %notfound)
	  or delete $state->{rejected};
    }

    keys %{$state->{rejected}};
}


#- parallel copy
sub parallel_register_rpms {
    my ($parallel, $urpm, @files) = @_;

    $parallel->copy_to_dir($urpm, @files, "$urpm->{cachedir}/rpms");

    #- keep trace of direct files.
    $parallel->{line} .= 
      join(' ',
	   map { "'$_'" }
	   map { "$urpm->{cachedir}/rpms/" . basename($_) } @files);
}

sub _find_remove_pre {
    my ($urpm, $state, %options) = @_;

    #- keep in mind if the previous selection is still active, it avoids
    #- to re-start urpme --test on each node.
    if ($options{find_packages_to_remove}) {
	delete $state->{rejected};
	delete $urpm->{error_remove};
	'--test ';
    } elsif (@{$urpm->{error_remove} || []}) {
	undef, $urpm->{error_remove};
    } elsif ($options{test}) {
	#- no need to restart what has been started before.
	undef, [ keys %{$state->{rejected}} ];
    } else {
	'--force ';
    }
}

sub _parse_urpme_output {
    my ($urpm, $state, $node, $s, $notfound, $base_to_remove, $bad_nodes, %options) = @_;

    $s =~ /^\s*$/ and return;
    $s =~ /Checking to remove the following packages/ and return;

    $s =~ /To satisfy dependencies, the following packages are going to be removed/
      and $urpm->{fatal}(1, N("node %s has an old version of urpme, please upgrade", $node));

    if ($s =~ /unknown packages?:? (.*)/) {
	#- remember unknown packages from the node, because it should not be a fatal error
	#- if other nodes have it.
	$notfound->{$_} = undef foreach split ", ", $1;
    } elsif ($s =~ /The following packages contain ([^:]*): (.*)/) {
	$options{callback_fuzzy} && $options{callback_fuzzy}->($urpm, $1, split(" ", $2))
	  or delete($state->{rejected}), return 'stop_parse';
    } elsif ($s =~ /removing package (.*) will break your system/) {
	$base_to_remove->{$1} = undef;
    } elsif ($s =~ /removing \S/) {
	#- this is log for newer urpme, so do not try to remove removing...
    } elsif ($s =~ /Remov(?:al|ing) failed/) {
	$bad_nodes->{$node} = [];
    } else {
	if (exists $bad_nodes->{$node}) {
	    $s =~ /^\s+(.+)/ and push @{$bad_nodes->{$node}}, $1;
	} else {
	    $s =~ s/\s*\(.*//; #- remove reason (too complex to handle, needs to be removed)
	    $state->{rejected}{$s}{removed} = 1;
	    $state->{rejected}{$s}{nodes}{$node} = undef;
	}
    }
    return;
}

sub _parse_urpmq_output {
    my ($urpm, $state, $node, $s, $cont, $chosen, %options) = @_;

    chomp $s;

    if (my ($action, $what) = $s =~ /^\@([^\@]*)\@(.*)/) {
	if ($action eq 'removing') {
	    $state->{rejected}{$what}{removed} = 1;
	    $state->{rejected}{$what}{nodes}{$node} = undef;
	}
    } elsif ($s =~ /\|/) {
	#- distant urpmq returned a choices, check if it has already been chosen
	#- or continue iteration to make sure no more choices are left.
	$$cont ||= 1; #- invalid transitory state (still choices is strange here if next sentence is not executed).
	unless (grep { exists $chosen->{$_} } split /\|/, $s) {
	    my $choice = $options{callback_choices}->($urpm, undef, $state, [ map { $urpm->search($_) } split /\|/, $s ]);
	    if ($choice) {
		$chosen->{scalar $choice->fullname} = $choice;
		#- it has not yet been chosen so need to ask user.
		$$cont = 2;
	    } else {
		#- no choices resolved, so forget it (no choices means no choices at all).
		$$cont = 0;
	    }
	}
    } else {
	my $pkg = $urpm->search($s) or return; #TODO
	$state->{selected}{$pkg->id}{$node} = $s;
    }
}

#- parallel resolve_dependencies
sub parallel_resolve_dependencies {
    my ($parallel, $synthesis, $urpm, $state, $requested, %options) = @_;

    #- first propagate the synthesis file to all machines
    $parallel->propagate_file($urpm, $synthesis);

    $parallel->{synthesis} = $synthesis;

    my $line = _simple_resolve_dependencies($parallel, $urpm, $state, $requested, %options);

    #- execute urpmq to determine packages to install.
    my ($cont, %chosen);
    do {
	$cont = 0; #- prepare to stop iteration.
	#- the following state should be cleaned for each iteration.
	delete $state->{selected};
	#- now try an iteration of urpmq.
	$parallel->urpm_popen($urpm, 'urpmq', "--synthesis $synthesis -fmc $line " . join(' ', keys %chosen), sub {
	    my ($node, $s) = @_;
	    _parse_urpmq_output($urpm, $state, $node, $s, \$cont, \%chosen, %options);
	});
	#- check for internal error of resolution.
	$cont == 1 and die "internal distant urpmq error on choice not taken";
    } while $cont;

    #- keep trace of what has been chosen finally (if any).
    $parallel->{line} = join(' ', $line, keys %chosen);
}

#- compute command line of urpm? tools.
sub _simple_resolve_dependencies {
    my ($parallel, $urpm, $state, $requested, %options) = @_;

    my @pkgs;
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
		push @pkgs, $choice;
	    }
	} else {
	    my $pkg = $urpm->{depslist}[$_] or next;
	    push @pkgs, $pkg;
	}
    }
    #- local packages have already been added.
    @pkgs = grep { !$urpm->{source}{$_->id} } @pkgs;

    $parallel->{line} . 
	($options{auto_select} ? ' --auto-select' : '') . 
	($options{keep} ? ' --keep' : '') .
	join(' ', map { scalar $_->fullname } @pkgs);
}

sub parallel_install {
    my ($parallel, $urpm, undef, $install, $upgrade, %options) = @_;

    $parallel->copy_to_dir($urpm, values %$install, values %$upgrade, "$urpm->{cachedir}/rpms");

    my (%bad_nodes, @good_nodes);
    $parallel->urpm_popen($urpm, 'urpmi', "--pre-clean --test --no-verify-rpm --auto --synthesis $parallel->{synthesis} $parallel->{line}", sub {
	my ($node, $s) = @_;
	$s =~ /^\s*$/ and return;
	$bad_nodes{$node} .= $s;
	$s =~ /Installation failed/ and $bad_nodes{$node} = '';
	$s =~ /Installation is possible/ and push @good_nodes, $node;
	undef;
    });
    delete $bad_nodes{$_} foreach @good_nodes;

    foreach (keys %{$parallel->{nodes}}) {
	exists $bad_nodes{$_} or next;
	$urpm->{error}(N("Installation failed on node %s", $_) . ":\n" . $bad_nodes{$_});
    }
    %bad_nodes and return;

    if ($options{test}) {
	$urpm->{error}(N("Installation is possible"));
	1;
    } else {
	my $line = $parallel->{line} . ($options{excludepath} ? " --excludepath '$options{excludepath}'" : "");
	#- continue installation.
	$parallel->run_urpm_command($urpm, 'urpmi', "--no-verify-rpm --auto --synthesis $parallel->{synthesis} $line");
    }
}

1;
