# Installation directories
PREFIX ?= $(DESTDIR)
localedir = $(PREFIX)/usr/share/locale

PGOAL = urpmi

# perl files to search translatable strings in
PL_FILES = ../rpm-find-leaves ../urpm.pm ../urpme ../urpmf \
	../urpmi ../urpmi.addmedia ../gurpmi ../gurpmi2 ../gurpmi.pm \
	../urpmi.removemedia ../urpmi.update ../urpmq ../urpmi.recover \
	../urpm/*.pm ../rurpmi

POFILES = $(shell ls *.po)
MOFILES = $(POFILES:%.po=%.mo)
LANGS = $(POFILES:%.po=%)

GOALS = $(PGOAL).pot $(MOFILES)

all: $(GOALS)

%.mo: %.po
	msgfmt -o $@ $<

update_n_merge: $(PGOAL).pot merge

merge:
	@for n in $(POFILES); do \
	    echo "Merging $$n"; \
	    msgmerge "$$n" $(PGOAL).pot > "$$n"t; \
	    mv -f "$$n"t "$$n"; \
	done

# there are better ways to do this, but..
merge_mageia_translations: $(PGOAL).pot
	@for n in $(POFILES); do \
		echo "Merging $$n translations from Mageia"; \
		mv "$$n" "$$n.orig"; \
		git checkout --theirs mageia/master "$$n"; \
		if [ $$? -eq 0 ]; then \
			sed -e '/^$$/q' "$$n.orig" > "$$n.header"; \
			sed -e 's#mageia#openmandriva#g' -i "$$n"; \
			msgcat --use-first -o "$$n.temp" -t utf-8 -s "$$n.orig" "$$n"; \
			git checkout --theirs remotes/origin/HEAD "$$n"; \
			msgcat --use-first -o "$$n.orig" -t utf-8 -s "$$n.header" "$$n.temp"; \
			rm "$$n.temp" "$$n.header"; \
		fi; \
		mv "$$n.orig" "$$n"; \
		msgmerge "$$n" $^ > "$$n"t; \
		mv -f "$$n"t "$$n"; \
	done

$(PGOAL).pot: $(PL_FILES) POTFILES.in
	intltool-update --gettext-package non-perl --pot
	perl_checker -q --generate-pot .$@ $(PL_FILES)
	msgcat -w 2048 --use-first .$@ non-perl.pot > $@
	rm -f non-perl.pot .$@

POTFILES.in:
	(cd .. ; /bin/ls -1 *.desktop.in polkit/*.policy.in; ) > POTFILES.in

install: all
	for l in $(LANGS); do \
	    install -d $(localedir)/$$l/LC_MESSAGES; \
	    install -m 644 $$l.mo $(localedir)/$$l/LC_MESSAGES/$(PGOAL).mo; \
	done

clean:
	@rm -rf *~ *.mo $(GOALS) $(PGOAL)_tmp.pot POTFILES.in

test:

realclean: clean
