.PHONY: default

default: bearriver.pdf

%.pdf: %.ps
	ps2pdf $<

%.ps: %.hp
	hp2ps $<
