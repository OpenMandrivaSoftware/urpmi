package urpm::get_pkgs;

# $Id$

use urpm::msg;
use urpm::sys;
use urpm::util;
use urpm::media;
use urpm 'file_from_local_url';


sub clean_all_cache {
    my ($urpm) = @_;
    #- clean download directory, do it here even if this is not the best moment.
    $urpm->{log}(N("cleaning %s and %s", "$urpm->{cachedir}/partial", "$urpm->{cachedir}/rpms"));
    urpm::sys::clean_dir("$urpm->{cachedir}/partial");
    urpm::sys::clean_dir("$urpm->{cachedir}/rpms");
}

#- select sources for selected packages,
#- according to keys of the packages hash.
#- returns a list of lists containing the source description for each rpm,
#- matching the exact number of registered media; ignored media being
#- associated to a null list.
sub selected2list {
    my ($urpm, $packages, %options) = @_;
    my (%protected_files, %local_sources, %fullname2id, %id_map);

    #- build association hash to retrieve id and examine all list files.
    foreach (keys %$packages) {
	foreach my $id (split /\|/, $_) {
	    if ($urpm->{source}{$_}) {
		my $file = $local_sources{$id} = $urpm->{source}{$id};
		$protected_files{$file} = undef;
	    } else {
		my $pkg = $urpm->{depslist}[$id];
		my $fullname = $pkg->fullname;
		my @pkgs = map { $_->id } grep { $fullname eq $_->fullname } $urpm->packages_by_name($pkg->name);

		# id_map is a remapping of id.
		# it is needed because @list must be [ { id => pkg } ] where id is one the selected id,
		# not really the real package id
		$id_map{$_} = $id foreach @pkgs;

		push @{$fullname2id{$fullname2id}}, @pkgs;
	    }
	}
    }

    #- examine the local repository, which is trusted (no gpg or pgp signature check but md5 is now done).
    foreach my $filepath (glob("$urpm->{cachedir}/rpms/*")) {
	next if -d $filepath;

	if (! -s $filepath) {
	    unlink $filepath; #- this file should be removed or is already empty.
	} else {
	    my $filename = basename($filepath);
	    my ($fullname) = $filename =~ /(.*)\.rpm$/ or next;
	    if (my $id = delete $fullname2id{$fullname}) {
		$local_sources{$id} = $filepath;
	    } else {
		$options{clean_other} && ! exists $protected_files{$filepath} and unlink $filepath;
	    }
	}
    }

    my @remaining_ids = sort { $a <=> $b } map { @$_ } values %fullname2id;

    my @list = map {
	my $medium = $_;
	my %sources;
	if (urpm::media::is_valid_medium($medium) && !$medium->{ignore}) {
	    while (@remaining_ids) {
		my $id = $remaining_ids[0];
		$medium->{start} <= $id && $id <= $medium->{end} or last;
		shift @remaining_ids;

		my $pkg = $urpm->{depslist}[$id];
		if ($pkg->filename !~ /\.delta\.rpm$/ || $urpm->is_delta_installable($pkg, $urpm->{root})) {
		    $sources{$id_map{$id}} = "$medium->{url}/" . $pkg->filename;
		}
	    }
	}
	\%sources;
    } (@{$urpm->{media} || []});

    if (@remaining_ids) {
	$urpm->{error}(N("package %s is not found.", $urpm->{depslist}[$_]->fullname)) foreach @remaining_ids;
	return;
    }

    (\%local_sources, \@list);
}

# TODO verify that files are downloaded from the right corresponding media
#- options: quiet, callback, 
sub download_packages_of_distant_media {
    my ($urpm, $list, $sources, $error_sources, %options) = @_;

    #- get back all ftp and http accessible rpm files into the local cache
    foreach my $n (0..$#$list) {
	my %distant_sources;

	#- ignore media that contain nothing for the current set of files
	values %{$list->[$n]} or next;

	#- examine all files to know what can be indexed on multiple media.
	while (my ($i, $url) = each %{$list->[$n]}) {
	    #- the given URL is trusted, so the file can safely be ignored.
	    defined $sources->{$i} and next;
	    my $local_file = file_from_local_url($url);
	    if ($local_file && $local_file =~ /\.rpm$/) {
		if (-r $local_file) {
		    $sources->{$i} = $local_file;
		} else {
		    $error_sources->{$i} = $local_file;
		}
	    } elsif ($url =~ m!^([^:]*):/(.*/([^/]*\.rpm))\Z!) {
		$distant_sources{$i} = "$1:/$2"; #- will download now
	    } else {
		$urpm->{error}(N("malformed URL: [%s]", $url));
	    }
	}

	if (%distant_sources && ! -w "$urpm->{cachedir}/partial") {
	    $urpm->{error}(N("sorry, you can't use --install-src to install remote .src.rpm files"));
	    exit 1;
	}

	#- download files from the current medium.
	if (%distant_sources) {
	    $urpm->{log}(N("retrieving rpm files from medium \"%s\"...", $urpm->{media}[$n]{name}));
	    if (urpm::download::sync($urpm, $urpm->{media}[$n], [ values %distant_sources ],
				     quiet => $options{quiet}, resume => $urpm->{options}{resume}, callback => $options{callback})) {
		$urpm->{log}(N("...retrieving done"));
	    } else {
		$urpm->{error}(N("...retrieving failed: %s", $@));
	    }
	    #- clean files that have not been downloaded, but keep in mind
	    #- there have been problems downloading them at least once, this
	    #- is necessary to keep track of failing downloads in order to
	    #- present the error to the user.
	    foreach my $i (keys %distant_sources) {
		my ($filename) = $distant_sources{$i} =~ m|/([^/]*\.rpm)$|;
		if ($filename && -s "$urpm->{cachedir}/partial/$filename" &&
		    URPM::verify_rpm("$urpm->{cachedir}/partial/$filename", nosignatures => 1))
		{
		    #- it seems the the file has been downloaded correctly and has been checked to be valid.
		    unlink "$urpm->{cachedir}/rpms/$filename";
		    urpm::util::move("$urpm->{cachedir}/partial/$filename", "$urpm->{cachedir}/rpms/$filename");
		    -r "$urpm->{cachedir}/rpms/$filename" and $sources->{$i} = "$urpm->{cachedir}/rpms/$filename";
		}
		unless ($sources->{$i}) {
		    $error_sources->{$i} = $distant_sources{$i};
		}
	    }
	}
    }

    #- clean failed download which have succeeded.
    delete @$error_sources{keys %$sources};

    1;
}

1;
