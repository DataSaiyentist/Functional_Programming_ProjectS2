prog: squelette.cmo source.cmo
	ocamlc -o $@ squelette.cmo source.cmo

squelette.cmo: squelette.ml
	ocamlc -c squelette.ml

source.cmo: squelette.cmo source.ml
	ocamlc -c source.ml

test.cmo: squelette.cmo source.cmo test.ml
	ocamlc -c test.ml

test: test.cmo source.cmo squelette.cmo
	ocamlc -o $@ squelette.cmo source.cmo test.cmo