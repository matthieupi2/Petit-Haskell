CMO = error.cmo lexer.cmo parser.cmo uncurriedAst.cmo main.cmo 
GENERATED = lexer.ml parser.ml parser.mli

all: petitghc

petitghc: $(CMO)
	ocamlc -o $@ $(CMO)

.SUFFIXES: .mli .ml .cmi .cmo

.mli.cmi:
	ocamlc -c $<

.ml.cmo:
	ocamlc -c $<

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.ml: error.cmo parser.mly ast.cmi
	menhir --infer parser.mly
parser.mli: error.cmo parser.mly ast.cmi
	menhir --infer parser.mly

clean:
	rm -f *.cm[io] *~ .depend petitghc main.native $(GENERATED)

.depend: $(GENERATED)
	ocamldep *.ml *.mli > .depend

include .depend
