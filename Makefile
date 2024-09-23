all: 
	ocamlc -c main.ml
	ocamlc -o chord main.cmo

clean:
	rm -f *.cmo *.cmi main