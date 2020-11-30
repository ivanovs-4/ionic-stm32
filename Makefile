.PHONY: all

all:
	./Build.hs

%:
	./Build.hs $@
