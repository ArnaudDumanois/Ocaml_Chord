all: 
	ocamlc -c main.ml
	ocamlc -o main main.cmo

clean:
	rm -f *.cmo *.cmi main