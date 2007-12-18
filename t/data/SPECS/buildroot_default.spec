%define macro_using_buildroot $(echo %buildroot)

Summary: x
Name: buildroot
Version: 1
Release: 1
License: x
Group: x
Url: x

%description
x

%package sub

Summary: x
Group: x
Version: 2
Release: 2

%description sub
x

%install
wanted=$(echo %{_tmppath}/buildroot-1-1-buildroot | sed 's!//!/!')
[ "%buildroot" = $wanted ] || { echo "buildroot should be $wanted instead of %buildroot"; exit 1; }
[ "$RPM_BUILD_ROOT" = $wanted ] || { echo "RPM_BUILD_ROOT should be $wanted instead of $RPM_BUILD_ROOT"; exit 1; }
[ "%macro_using_buildroot" = $wanted ] || { echo "macro_using_buildroot should be $wanted instead of %buildroot"; exit 1; }

install -d $RPM_BUILD_ROOT/etc
echo foo > $RPM_BUILD_ROOT/etc/foo

%files
/etc/foo
