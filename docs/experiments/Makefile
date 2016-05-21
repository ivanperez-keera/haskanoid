.PHONY: default

default: haskanoid.pdf

%.pdf: %.ps
	ps2pdf $<

%.ps: %.hp
	hp2ps $<
