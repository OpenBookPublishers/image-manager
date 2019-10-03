
all: obparch.png image-manager.pdf

obparch.png: obparch.dot
	dot -Tpng $< > $@

%.pdf: %.md
	md2pdf $<

