GENERATED = lexer.ml parser.ml parser.mli parser.output parser.automaton petitghc

clean:
	rm -f .{[!g]*,g[!i]*,gi[!t]*} *.cm[iox] *.o *.annot *~ *.native $(GENERATED)
