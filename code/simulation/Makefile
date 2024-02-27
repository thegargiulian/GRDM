#
# Authors:     MG
# Maintainers: MG, HG
# =========================================
# GRDM/code/method-types/Makefile

.PHONY: all clean

all: output/grdm-section-3-figure.png

clean:
	-rm output/*

output/grdm-section-3-figure.png: \
		src/simulation.R
	-mkdir output
	Rscript --vanilla $<

# done.
