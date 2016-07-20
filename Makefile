OCAMLOPT=ocamlfind ocamlopt
OCAMLC=ocamlfind ocamlc

OCAMLDEP=ocamlfind ocamldep

COMPFLAG=-package ppx_sexp_conv -g

RESULT=min-caml

OBJS=syntax.cmo parser.cmo lexer.cmo \
  id.cmo type.cmo env.cmo typedtree.cmo typing.cmo \
  main.cmo

XOBJS=$(OBJS:.cmo=.cmx)


$(RESULT): $(OBJS)
	$(OCAMLC) $(COMPFLAG) -linkall -linkpkg -o $@ $^

$(RESULT).opt: $(XOBJS)
	$(OCAMLOPT) $(COMPFLAG) -linkall -linkpkg -o $@ $^

parser.ml:
	ocamlyacc -v parser.mly

lexer.ml:
	ocamllex lexer.mll

depend:
	$(OCAMLDEP) *.ml *.mli > .depend

type_test: $(RESULT)
	for d in `ls test/*.ml`; do \
	echo $$d; ./min-caml "$${d%.*}"; \
	echo "#############"; \
	done

clean:
	rm -rf *.cm? *.o $(RESULT) $(RESULT).opt parser.ml lexer.ml

.SUFFIXES: .ml .mli .cmo .cmi .cmx .o

%.cmo: %.ml
	$(OCAMLC) $(COMPFLAG) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(COMPFLAG) -c $<

%.cmi: %.mli
	$(OCAMLC) $(COMPFLAG) -c $<

%.ml: %.mly
	ocamlyacc -v $<
%.ml: %.mll
	ocamllex $<

include .depend
