PHONY : all clean test

all : parsing

%.lex.sml : %.lex
	mllex $<

%.grm.sml : %.grm
	mlyacc $<

tigh: tokens.sml tiger.lex.sml tiger.mlb tigh.sml
	mlton -output tigh tiger.mlb

parsing: ast.sml tiger.sml tiger.mlb expr.grm.sml tiger.lex.sml indent.sml
	mlton -output parsing tiger.mlb

clean : 
	rm -f parsing
	rm -f tiger.lex.sml

test: 
	$(CURDIR)/parsing test.txt
