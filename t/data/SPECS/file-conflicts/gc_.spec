Summary: x
Name: gc_
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
install -d $RPM_BUILD_ROOT/etc/dir_symlink
echo a > $RPM_BUILD_ROOT/etc/dir_symlink/a

%clean
rm -rf $RPM_BUILD_ROOT

%files
/etc/dir_symlink/a
