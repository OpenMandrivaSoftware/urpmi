Summary: c
Name: c
Version: 1
Release: 1
License: x
Group: x
Url: x
BuildRoot: %{_tmppath}/%{name}
Conflicts: a

%description
x

%build
rm -rf $RPM_BUILD_ROOT
echo "installing %name" > README.install.urpmi
echo "upgrading %name" > README.upgrade.urpmi

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc README.install.urpmi README.upgrade.urpmi
