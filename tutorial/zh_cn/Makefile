# A simple makefile that generates image to post on weibo.com.
# It requires webkit2png, pandoc, and netpbm tools.
default:   
	@echo Use 'make <doc>.open' or 'make <doc>.png'

%.open:   %.png
	open $<

%.html:  %.md tutorial.css
	pandoc -s -c tutorial.css $< > $@

%.png-full.png: %.html
	webkit2png.py -o $(<:.html=.png) file://`pwd`/$<

%.png: %.png-full.png
	pngtopnm $< |pnmcrop|pnmcrop -bottom -margin=30|pnmtopng > $@
