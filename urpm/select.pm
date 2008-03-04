package urpm::select;

# $Id$

use urpm::msg;
use urpm::util;
use urpm::sys;
use URPM;

my $default_list = 'rpm,perl-URPM,perl-MDV-Distribconf,urpmi,meta-task,glibc';

sub add_packages_to_priority_upgrade_list {
    my (@packages) = @_;
    @packages = grep { $default_list !~ /,$_\b/ } @packages;
    return if !@packages;
    $default_list .= join(',', '', @packages);
}

sub set_priority_upgrade_option {
    my ($urpm, $previous) = @_;

    exists $urpm->{options}{'priority-upgrade'} and return;

    # comma-separated list of packages that should be installed first,
    # and that trigger an urpmi restart
    my $list = $default_list;
    if ($previous) {
	if ($previous eq $list) {
	    $list = '';
	    $urpm->{log}(N("urpmi was restarted, and the list of priority packages did not change"));
	} else {
	    $urpm->{log}(N("urpmi was restarted, and the list of priority packages did change: %s vs %s", $previous, $list));
	}
    }
    $urpm->{options}{'priority-upgrade'} = $list;
}

sub _findindeps {
    my ($urpm, $found, $qv, $v, $caseinsensitive, $src) = @_;

    foreach (keys %{$urpm->{provides}}) {
	#- search through provides to find if a provide matches this one;
	#- but manage choices correctly (as a provides may be virtual or
	#- defined several times).
	/$qv/ || !$caseinsensitive && /$qv/i or next;

	my @list = grep { defined $_ } map {
	    my $pkg = $_;
	    $pkg && ($src ? $pkg->arch eq 'src' : $pkg->arch ne 'src')
	      ? $pkg->id : undef;
	} $urpm->packages_providing($_);
	@list > 0 and push @{$found->{$v}}, join '|', @list;
    }
}

sub pkg_in_searchmedia {
    my ($urpm, $pkg) = @_;

    foreach my $medium (grep { $_->{searchmedia} } @{$urpm->{media}}) {
	$medium->{start} <= $pkg->id
	  && $medium->{end} >= $pkg->id and return 1;
    }
    0;
}
sub searchmedia_idlist {
    my ($urpm) = @_;
    $urpm->{searchmedia} && [ 
	map { $_->{start} .. $_->{end} } 
	  grep { $_->{searchmedia} } @{$urpm->{media}}
    ];
}
sub build_listid_ {
    my ($urpm) = @_;
    $urpm->build_listid(undef, undef, searchmedia_idlist($urpm));
}

#- search packages registered by their names by storing their ids into the $packages hash.
#- Recognized options:
#-	all
#-	caseinsensitive
#-	fuzzy
#-	src
#-	use_provides
#-
#- side-effects: $packages, flag_skip
sub search_packages {
    my ($urpm, $packages, $names, %options) = @_;

    my ($name2ids, $result) = _search_packages($urpm, $names, %options) or return;

    foreach my $v (@$names) {
	my @ids = split /\|/, $name2ids->{$v};

	#- in case we have a substring match, we want individual selection (for urpmq --fuzzy)
	$packages->{$_} = 1 foreach $result eq 'substring' || $options{all} ? @ids : $name2ids->{$v};

	foreach (@ids) {
	    my $pkg = $urpm->{depslist}[$_] or next;
	    $urpm->{debug} and $urpm->{debug}("search_packages: found " . $pkg->fullname . " matching $v");
	    $pkg->set_flag_skip(0); #- reset skip flag as manually selected.
	}
    }
    $result;
}

#- side-effects: none
sub _search_packages {
    my ($urpm, $names, %options) = @_;
    my (%exact, %exact_a, %exact_ra, %found, %foundi);
    foreach my $v (@$names) {
	my $qv = quotemeta $v;
	$qv = '(?i)' . $qv if $options{caseinsensitive};

	unless ($options{fuzzy}) {
	    #- try to search through provides.
	    if (my @l = map {
		    $_
		    && ($options{src} ? $_->arch eq 'src' : $_->is_arch_compat)
		    && ($options{use_provides} || $_->name eq $v)
		    && defined($_->id)
		    && (!$urpm->{searchmedia} || pkg_in_searchmedia($urpm, $_))
		    ? $_ : @{[]};
		} $urpm->packages_providing($v))
	    {
		#- find the lowest value of is_arch_compat
		my ($noarch, $arch) = partition { $_->arch eq 'noarch' } @l;
		my %compats;
		push @{$compats{$_->is_arch_compat}}, $_ foreach @$arch;

		delete $compats{0}; #- means not compatible
		#- if there are pkgs matching arch, prefer them
		if (%compats && !$options{all}) {
		    my $best_arch = min(keys %compats);
		    %compats = ($best_arch => $compats{$best_arch});
		}
		if (%compats) {
		    @l = (@$noarch, map { @$_ } values %compats);
		}

		#- we assume that if there is at least one package providing
		#- the resource exactly, this should be the best one; but we
		#- first check if one of the packages has the same name as searched.
		if (my @l2 = grep { $_->name eq $v } @l) {
		    @l = @l2;
		}
		$exact{$v} = join('|', map { $_->id } @l);
		next;
	    }
	}

	if ($options{use_provides} && $options{fuzzy}) {
	    _findindeps($urpm, \%found, $qv, $v, $options{caseinsensitive}, $options{src});
	}

	foreach my $id (build_listid_($urpm)) {
	    my $pkg = $urpm->{depslist}[$id];
	    ($options{src} ? $pkg->arch eq 'src' : $pkg->is_arch_compat) or next;
	    my $pack_name = $pkg->name;
	    my $pack_ra = $pack_name . '-' . $pkg->version;
	    my $pack_a = "$pack_ra-" . $pkg->release;
	    my $pack = "$pack_a." . $pkg->arch;
	    unless ($options{fuzzy}) {
		if ($pack eq $v) {
		    $exact{$v} = $id;
		    next;
		} elsif ($pack_a eq $v) {
		    push @{$exact_a{$v}}, $id;
		    next;
		} elsif ($pack_ra eq $v || $options{src} && $pack_name eq $v) {
		    push @{$exact_ra{$v}}, $id;
		    next;
		}
	    }
	    $pack =~ /$qv/ and push @{$found{$v}}, $id;
	    $pack =~ /$qv/i and push @{$foundi{$v}}, $id unless $options{caseinsensitive};
	}
    }

    my $result = 1;
    my %name2ids;
    foreach my $v (@$names) {
	if (defined $exact{$v}) {  
	    $name2ids{$v} = $exact{$v};
	} else {
	    #- at this level, we need to search the best package given for a given name,
	    #- always prefer already found package.
	    my %l;
	    foreach (@{$exact_a{$v} || $exact_ra{$v} || $found{$v} || $foundi{$v} || []}) {
		my $pkg = $urpm->{depslist}[$_];
		push @{$l{$pkg->name}}, $pkg;
	    }
	    if (values(%l) == 0 || values(%l) > 1 && !$options{all}) {
		$urpm->{error}(N("No package named %s", $v));
		values(%l) != 0 and $urpm->{error}(
		    N("The following packages contain %s: %s",
			$v, "\n" . join("\n", sort { $a cmp $b } keys %l))
		);
		$result = 0;
	    } else {
		if (!@{$exact_a{$v} || $exact_ra{$v} || []}) {
		    #- we found a non-exact match
		    $result = 'substring';
		}
		$name2ids{$v} = join('|', map {
		    my $best;
		    foreach (@$_) {
			if ($best && $best != $_) {
			    $_->compare_pkg($best) > 0 and $best = $_;
			} else {
			    $best = $_;
			}
		    }
		    map { $_->id } grep { $_->fullname eq $best->fullname } @$_;
		} values %l);
	    }
	}
    }

    #- return 0 if error, 'substring' if fuzzy match, 1 if ok
    \%name2ids, $result;
}

#- Resolves dependencies between requested packages (and auto selection if any).
#- handles parallel option if any.
#- The return value is true if program should be restarted (in order to take
#- care of important packages being upgraded (priority upgrades)
#- %options :
#-	rpmdb
#-	auto_select
#-	install_src
#-	priority_upgrade
#-	upgrade_callback
#-	resolve_req_callback
#- %options passed to ->resolve_requested:
#-	callback_choices
#-	keep
#-	nodeps
#-	no_suggests
sub resolve_dependencies {
    #- $state->{selected} will contain the selection of packages to be
    #- installed or upgraded
    my ($urpm, $state, $requested, %options) = @_;
    my $need_restart;

    if ($urpm->{parallel_handler}) {
	require urpm::parallel; #- help perl_checker;
	urpm::parallel::resolve_dependencies($urpm, $state, $requested, %options);
    } else {
	my $db;

	if ($options{rpmdb}) {
	    $db = new URPM;
	    $db->parse_synthesis($options{rpmdb});
	} else {
	    $db = urpm::db_open_or_die($urpm, $urpm->{root});
	}

	my $sig_handler = sub { undef $db; exit 3 };
	local $SIG{INT} = $sig_handler;
	local $SIG{QUIT} = $sig_handler;

	#- auto select package for upgrading the distribution.
	if ($options{auto_select}) {
	    $urpm->request_packages_to_upgrade($db, $state, $requested, requested => undef,
					       $urpm->{searchmedia} ? (idlist => searchmedia_idlist($urpm)) : (),
					   );
            $options{upgrade_callback} and $options{upgrade_callback}->();
	}

	my @priority_upgrade;
	my $resolve_priority_upgrades = sub {
	    my ($selected, $priority_requested) = @_;
 
	    my %priority_state;

	    $urpm->resolve_requested__no_suggests_($db, \%priority_state, $priority_requested, %options);
	    if (grep { ! exists $priority_state{selected}{$_} } keys %$priority_requested) {
		#- some packages which were selected previously have not been selected, strange!
	    } elsif (grep { ! exists $priority_state{selected}{$_} } keys %$selected) {
		#- there are other packages to install after this priority transaction.
		%$state = %priority_state;
		$need_restart = 1;
	    }
	};

	if ($options{priority_upgrade} && !$options{rpmdb}) {
	    @priority_upgrade = map {
		$urpm->packages_by_name($_);
	    } split(/,/, $options{priority_upgrade});

	    #- first check if a priority_upgrade package is requested
	    #- (it should catch all occurences in --auto-select mode)
	    #- (nb: a package "foo" may appear twice, and only one will be set flag_upgrade)
	    if (my @l = grep { $_->flag_upgrade } @priority_upgrade) {
		my %priority_requested = map { $_->id => undef } @l;
		$resolve_priority_upgrades->($requested, \%priority_requested);
	    }
	}

	if (!$need_restart) {
	    my @requested = $urpm->resolve_requested($db, $state, $requested, %options);
	    $options{resolve_req_callback} and $options{resolve_req_callback}->(@requested);

	    #- now check if a priority_upgrade package has been required
	    #- by a requested package
	    if (my @l = grep { $state->{selected}{$_->id} } @priority_upgrade) {
		my %priority_requested = map { $_->id => undef } @l;
		$resolve_priority_upgrades->($state->{selected}, \%priority_requested);
	    }
	}
    }
    $need_restart;
}

sub cooked_prefer {
    my ($urpm, $cmdline_prefer) = @_;

    $urpm->{prefer_regexps} ||= [
	map {
	    m!^/(.*)/$! ? "($1)" : '^' . quotemeta($_) . '$';
	} map { @$_ }
	  urpm::sys::get_packages_list($urpm->{prefer_list}, $cmdline_prefer),
	  urpm::sys::get_packages_list($urpm->{prefer_vendor_list})
    ];
    @{$urpm->{prefer_regexps}};
}

sub get_preferred {
    my ($urpm, $choices, $cmdline_prefer) = @_;

    my @prefer;
    my @l = @$choices;
    foreach my $re (cooked_prefer($urpm, $cmdline_prefer)) {
	my ($prefer, $other) = partition { $_->name =~ $re } @l;
	push @prefer, @$prefer;
	@l = @$other;

	if (@$prefer) {
	    my $prefer_s = join(',', map { $_->name } @$prefer);
	    my $other_s     = join(',', map { $_->name } @l);
	    $urpm->{log}("preferring $prefer_s over $other_s");
	}
    }
    
    #- only keep the best prefered
    #- then put the other prefered packages first 
    my $best = shift @prefer; 
    $best ? [$best] : [], [@prefer, @l];
}

#- find packages to remove.
#- options:
#-	callback_base
#-	callback_fuzzy
#-	callback_notfound
#-	force
#-	matches
#-	test
sub find_packages_to_remove {
    my ($urpm, $state, $l, %options) = @_;

    if ($urpm->{parallel_handler}) {
	#- invoke parallel finder.
	$urpm->{parallel_handler}->parallel_find_remove($urpm, $state, $l, %options, find_packages_to_remove => 1);
    } else {
	my $db = urpm::db_open_or_die($urpm, $urpm->{root});
	my (@m, @notfound);

	if (!$options{matches}) {
	    foreach (@$l) {
		my ($n, $found);

		#- check if name-version-release.architecture was given.
		if (($n) = /^(.*)-[^\-]*-[^\-]*\.[^\.\-]*$/) {
		    $db->traverse_tag('name', [ $n ], sub {
			    my ($p) = @_;
			    $p->fullname eq $_ or return;
			    $urpm->resolve_rejected($db, $state, $p, removed => 1);
			    push @m, scalar $p->fullname;
			    $found = 1;
			});
		    $found and next;
		}

		#- check if name-version-release was given.
		if (($n) = /^(.*)-[^\-]*-[^\-]*$/) {
		    $db->traverse_tag('name', [ $n ], sub {
			    my ($p) = @_;
			    my ($name, $version, $release) = $p->fullname;
			    "$name-$version-$release" eq $_ or return;
			    $urpm->resolve_rejected($db, $state, $p, removed => 1);
			    push @m, scalar $p->fullname;
			    $found = 1;
			});
		    $found and next;
		}

		#- check if name-version was given.
		if (($n) = /^(.*)-[^\-]*$/) {
		    $db->traverse_tag('name', [ $n ], sub {
			    my ($p) = @_;
			    my ($name, $version) = $p->fullname;
			    "$name-$version" eq $_ or return;
			    $urpm->resolve_rejected($db, $state, $p, removed => 1);
			    push @m, scalar $p->fullname;
			    $found = 1;
			});
		    $found and next;
		}

		#- check if only name was given.
		$db->traverse_tag('name', [ $_ ], sub {
			my ($p) = @_;
			$p->name eq $_ or return;
			$urpm->resolve_rejected($db, $state, $p, removed => 1);
			push @m, scalar $p->fullname;
			$found = 1;
		    });
		$found and next;

		push @notfound, $_;
	    }
	    if (!$options{force} && @notfound && @$l > 1) {
		$options{callback_notfound} && $options{callback_notfound}->($urpm, @notfound)
		  or return ();
	    }
	}
	if ($options{matches} || @notfound) {
	    my $match = join "|", map { quotemeta } @$l;
	    my $qmatch = qr/$match/;

	    #- reset what has been already found.
	    %$state = ();
	    @m = ();

	    $urpm->{log}(qq(going through installed packages looking for "$match"...));
	    #- search for packages that match, and perform closure again.
	    $db->traverse(sub {
		    my ($p) = @_;
		    my $f = scalar $p->fullname;
		    $f =~ $qmatch or return;
		    $urpm->resolve_rejected($db, $state, $p, removed => 1);
		    push @m, $f;
		});
	    $urpm->{log}("...done, packages found [" . join(' ', @m) . "]");

	    if (!$options{force} && @notfound) {
		if (@m) {
		    $options{callback_fuzzy} && $options{callback_fuzzy}->($urpm, @$l > 1 ? $match : $l->[0], @m)
		      or return ();
		} else {
		    $options{callback_notfound} && $options{callback_notfound}->($urpm, @notfound)
		      or return ();
		}
	    }
	}

	#- check if something needs to be removed.
	find_removed_from_basesystem($urpm, $db, $state, $options{callback_base})
	    or return ();
    }
    removed_packages($urpm, $state);
}

sub find_removed_from_basesystem {
    my ($urpm, $db, $state, $callback_base) = @_;
    if ($callback_base && %{$state->{rejected} || {}}) {
	my %basepackages;
	my @dont_remove = ('basesystem', 'basesystem-minimal', 
			   split /,\s*/, $urpm->{global_config}{'prohibit-remove'});
	#- check if a package to be removed is a part of basesystem requires.
	$db->traverse_tag('whatprovides', \@dont_remove, sub {
	    my ($p) = @_;
	    $basepackages{$p->fullname} = 0;
	});
	foreach (removed_packages($urpm, $state)) {
	    exists $basepackages{$_} or next;
	    ++$basepackages{$_};
	}
	if (grep { $_ } values %basepackages) {
	    return $callback_base->($urpm, grep { $basepackages{$_} } keys %basepackages);
	}
    }
    return 1;
}

#- misc functions to help finding ask_unselect and ask_remove elements with their reasons translated.
sub unselected_packages {
    my (undef, $state) = @_;
    grep { $state->{rejected}{$_}{backtrack} } keys %{$state->{rejected} || {}};
}

sub translate_why_unselected {
    my ($urpm, $state, @fullnames) = @_;

    join("\n", map { translate_why_unselected_one($urpm, $state, $_) } sort @fullnames);
}

sub translate_why_unselected_one {
    my ($urpm, $state, $fullname) = @_;

    my $obj = $state->{rejected}{$fullname};
    my $rb = $obj->{backtrack};
    my @froms = keys %{$rb->{closure} || {}};
    my @unsatisfied = @{$rb->{unsatisfied} || []};
    my @conflicts = keys(%$rb) ? () : keys %{$obj->{closure}};
    my $s = join ", ", (
	(map { N("due to missing %s", $_) } @froms),
	(map { N("due to already installed %s", $_) } @conflicts),
	(map { N("due to unsatisfied %s", $_) } uniq(map {
	    #- XXX in theory we shouldn't need this, dependencies (and not ids) should
	    #- already be present in @unsatisfied. But with biarch packages this is
	    #- not always the case.
	    /\D/ ? $_ : scalar($urpm->{depslist}[$_]->fullname);
	} @unsatisfied)),
	$rb->{promote} && !$rb->{keep} ? N("trying to promote %s", join(", ", @{$rb->{promote}})) : (),
	$rb->{keep} ? N("in order to keep %s", join(", ", @{$rb->{keep}})) : (),
    );
    $fullname . ($s ? " ($s)" : '');
}

sub removed_packages {
    my (undef, $state) = @_;
    grep {
	$state->{rejected}{$_}{removed} && !$state->{rejected}{$_}{obsoleted};
    } keys %{$state->{rejected} || {}};
}

sub translate_why_removed {
    my ($urpm, $state, @fullnames) = @_;
    join("\n", map { translate_why_removed_one($urpm, $state, $_) } sort @fullnames);
}
sub translate_why_removed_one {
    my ($urpm, $state, $fullname) = @_;

    my $closure = $state->{rejected} && $state->{rejected}{$fullname} && $state->{rejected}{$fullname}{closure}
      or return $fullname;

    my ($from) = keys %$closure;
    my ($whyk) = keys %{$closure->{$from}};
    my $whyv = $closure->{$from}{$whyk};
    my $frompkg = $urpm->search($from, strict_fullname => 1);
    my $s = do {
	if ($whyk =~ /old_requested/) {
	    N("in order to install %s", $frompkg ? scalar $frompkg->fullname : $from);
	} elsif ($whyk =~ /unsatisfied/) {
	    join(",\n  ", map {
		if (/([^\[\s]*)(?:\[\*\])?(?:\[|\s+)([^\]]*)\]?$/ && $2 ne '*') {
		    N("due to unsatisfied %s", "$1 $2");
		} else {
		    N("due to missing %s", $_);
		}
	    } @$whyv);
	} elsif ($whyk =~ /conflicts/) {
	    N("due to conflicts with %s", $whyv);
	} else {
	    $whyk;
	}
    };
    #- now insert the reason if available.
    $fullname . ($s ? "\n ($s)" : '');
}


1;
