GENERATED = lexer.ml parser.ml parser.mli parser.output parser.automaton petitghc

clean:
	rm -f *.cm[iox] *.o *.annot *~ *.native $(GENERATED)
	rm -R _build
