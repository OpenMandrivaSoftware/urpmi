Summary: b
Name: b
Version: 1
Release: 1
License: x
Group: x
Url: x
BuildRoot: %{_tmppath}/%{name}
Provides: a > 1
Obsoletes: a <= 1
Requires: c

%description
x

%install
rm -rf $RPM_BUILD_ROOT
install -d $RPM_BUILD_ROOT/etc
echo foo > $RPM_BUILD_ROOT/etc/foo

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
/etc/*
