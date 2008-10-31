Summary: x
Name: a
Version: 1
Release: 1
License: x
Group: x
Url: x
Provides: /bin/a
BuildRequires: gcc
BuildRoot: %{_tmppath}/%{name}

%prep
%setup -c -T
cat <<EOF > a.c 
#include <stdio.h>
int main(int argc, char **argv) { 
   FILE *f = fopen(argv[1], "r");
   int c; 
   while ((c = getc(f)) > 0) putchar(c); 
   putchar('\n');
   return 0;
}
EOF

%build
gcc -Wall -static -o a a.c

%install
rm -rf $RPM_BUILD_ROOT
install -D a $RPM_BUILD_ROOT/bin/a

%clean
rm -rf $RPM_BUILD_ROOT

%description
x

%files
%defattr(-,root,root)
/bin/*
