Summary: x
Name: d
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
install -d $RPM_BUILD_ROOT/etc/dir
echo d > $RPM_BUILD_ROOT/etc/dir/d

%clean
rm -rf $RPM_BUILD_ROOT

%files
/etc/*
