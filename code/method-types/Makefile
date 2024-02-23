#
# Authors:     MG
# Maintainers: MG
# =========================================
# GRDM/code/method-types/Makefile

.PHONY: all clean

all: graphs-done.touch

clean:
	-rm output/*

graphs-done.touch: \
		src/graph-methods.R
	-mkdir output
	Rscript --vanilla $<
	touch $@

# done.
