Summary: c
Name: c
Version: 1
Release: 1
License: x
Group: x
Url: x
BuildRoot: %{_tmppath}/%{name}
Conflicts: a <= 1

%description
x

%install
rm -rf $RPM_BUILD_ROOT
install -d $RPM_BUILD_ROOT/etc
echo bar > $RPM_BUILD_ROOT/etc/bar

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
/etc/*
