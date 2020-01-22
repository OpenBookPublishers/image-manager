
all: obparch.png image-manager.pdf using-image-manager.pdf

obparch.png: obparch.dot
	dot -Tpng $< > $@

%.pdf: %.md
	md2pdf $<

install: using-image-manager.pdf
	cp $^ ../app/imagemanager/client/public

