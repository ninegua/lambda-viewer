# A simple makefile that generates image to post on weibo.com.
# It requires Mac OS X, and the following tools: webkit2png, 
# pandoc, and netpbm. 

SRCS=00-preface.md \
	 01-lambda-expression.md \
	 02-data-type.md \
	 03-induction.md \
	 04-parser.md \
	 05-parser-combinator.md \
	 06-lambda-parser.md \
	 07-alpha-conversion.md \
	 08-beta-reduction.md

default:   
	@echo Use 'make <doc>.open' or 'make <doc>.png'

%.open: %.png
	open $<

%.html: %.md tutorial.css
	pandoc -s -c tutorial.css $< > $@

%.png-full.png: %.html
	webkit2png.py -o $(<:.html=.png) file://`pwd`/$<

%.png: %.png-full.png
	pngtopnm $< |pnmcrop|pnmcrop -bottom -margin=30|pnmtopng > $@

clean:
	@rm -f $(patsubst %.md, %.png, $(SRCS)) \
	       $(patsubst %.md, %.png-full.png, $(SRCS)) \
	       $(patsubst %.md, %.png-clipped.png, $(SRCS)) \
	       $(patsubst %.md, %.png-thumb.png, $(SRCS))

.SECONDARY: $(patsubst %.md, %.png, $(SRCS))

.PHONY: clean 

