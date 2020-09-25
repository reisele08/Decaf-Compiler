
lexlib=l
bindir=.
rm=/bin/rm -f
targets=
cpptargets=decaflex

all: $(targets) $(cpptargets)

$(targets): %: %.l
	@echo "compiling lex file:" $<
	@echo "output file:" $@
	flex -o$@.c $<
	gcc -o $(bindir)/$@ $@.c -l$(lexlib)
	$(rm) $@.c

$(cpptargets): %: %.l
	@echo "compiling cpp lex file:" $<
	@echo "output file:" $@
	flex -o$@.cc $<
	g++ -o $(bindir)/$@ $@.cc -l$(lexlib)
	$(rm) $@.cc

clean:
	$(rm) $(targets) $(cpptargets)

