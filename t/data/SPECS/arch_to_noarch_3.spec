Summary: arch_to_noarch
Name: arch_to_noarch
Version: 3
Release: 1
License: x
Group: x
Url: x
BuildRoot: %{_tmppath}/%{name}
BuildArch: noarch

%prep

%build

%install
rm -rf $RPM_BUILD_ROOT
install -d $RPM_BUILD_ROOT/usr/lib/test-%{name}
echo foo > $RPM_BUILD_ROOT/usr/lib/test-%{name}/foo

%clean
rm -rf $RPM_BUILD_ROOT

%description
this pkg is now a noarch

%files
%defattr(-,root,root)
%config(noreplace) /usr/lib/test-%{name}

