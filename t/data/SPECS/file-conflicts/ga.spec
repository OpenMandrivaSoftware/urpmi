Summary: x
Name: ga
Version: 1
Release: 1
License: x
Group: x
Url: x
BuildArch: noarch

%description
x

%install
rm -rf $RPM_BUILD_ROOT
install -d $RPM_BUILD_ROOT/etc
ln -s dir $RPM_BUILD_ROOT/etc/dir_symlink

%clean
rm -rf $RPM_BUILD_ROOT

%files
/etc/*
