OCAMLC=ocamlfind ocamlc
OCAMLOPT=ocamlfind ocamlopt
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
LIB=-package sexplib,ppx_sexp_conv
COMPFLAGS=$(LIB)

RESULT = min-caml
CC = gcc
CFLAGS = -g -O2 -Wall

OBJS= type.cmo id.cmo m.cmo s.cmo \
syntax.cmo parser.cmo lexer.cmo typing.cmo typing.cmo main.cmo

$(RESULT): $(OBJS)
	$(OCAMLC) $(COMPFLAGS) -o $@ -linkpkg -linkall $(OBJS)

$(RESULT).opt: $(XOBJS)
	$(OCAMLOPT) $(COMPFLAGS) -o $@ -linkpkg -linkall $(XOBJS)

clean::
	rm -rf *.cm* $(RESULT) $(RESULT).opt *.o *.cmt parser.ml lexer.ml


XOBJS=$(OBJS:.cmo=.cmx)

# ↓テストプログラムが増えたら、これも増やす
TESTS = print sum-tail gcd sum fib ack even-odd \
adder funcomp cls-rec cls-bug cls-bug2 cls-reg-bug \
shuffle spill spill2 spill3 join-stack join-stack2 join-stack3 \
join-reg join-reg2 non-tail-if non-tail-if2 \
inprod inprod-rec inprod-loop matmul matmul-flat

do_test: $(TESTS:%=test/%.cmp)

pack.cma: $(OBJS)
	$(OCAMLC) -a -o $@ $^

# .PRECIOUS: test/%.s test/% test/%.res test/%.ans test/%.cmp
# TRASH = $(TESTS:%=test/%.s) $(TESTS:%=test/%) $(TESTS:%=test/%.res) $(TESTS:%=test/%.ans) $(TESTS:%=test/%.cmp)

# test/%.s: $(RESULT) test/%.ml
# 	./$(RESULT) test/$*
# test/%: test/%.s libmincaml.S stub.c
# 	$(CC) $(CFLAGS) -m32 $^ -lm -o $@
# test/%.res: test/%
# 	$< > $@
# test/%.ans: test/%.ml
# 	ocaml $< > $@
# test/%.cmp: test/%.res test/%.ans
# 	diff $^ > $@

# release: min-caml.html
# 	rm -fr tmp ; mkdir tmp ; cd tmp ; cvs -d:ext:sumii@min-caml.cvs.sf.net://cvsroot/min-caml export -Dtomorrow min-caml ; tar cvzf ../min-caml.tar.gz min-caml ; cd .. ; rm -fr tmp
# 	cp Makefile stub.c SPARC/libmincaml.S min-caml.html min-caml.tar.gz ../htdocs/

# generic rules :
#################

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi .cmx

# Terrible hack to avoid conflict of `parser.cmi` in compiler-libs
parser.cmo: parser.ml parser.cmi
	ocamlc -c $<
parser.cmx: parser.ml parser.cmi
	ocamlopt -c $<

.ml.cmo:
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLPP) $(COMPFLAGS) -c $<

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) -v $<

.mly.mli:
	$(OCAMLYACC) -v $<

beforedepend::

depend: beforedepend
	ocamlfind ocamldep $(INCLUDES) *.mli *.ml > .depend

.PHONY: clean install installopt beforedepend depend test

include ./.depend
