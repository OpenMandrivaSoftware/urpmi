%define foobar echo foo > $RPM_BUILD_ROOT/etc/foo \
               echo bar > $RPM_BUILD_ROOT/etc/bar

Summary: x
Name: multi-linux-macro
Version: 1
Release: 1
License: x
Group: x
Url: x
BuildRoot: %{_tmppath}/%{name}

%clean
rm -rf $RPM_BUILD_ROOT

%description
x

%files
%defattr(-,root,root)
/etc/foo
/etc/bar

%install
rm -rf $RPM_BUILD_ROOT
install -d $RPM_BUILD_ROOT/etc
%foobar
