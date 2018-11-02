
CC=ocamlc

OPT= 

TARGETS=utils.cmo prog.ml

LIBS=graphics.cma

all: $(TARGETS)
	$(CC) $(OPT) $(LIBS) $(TARGETS) -o prog
utils.cmo:
	$(CC) $(OPT) -c utils.ml
clear:
	rm -f *.cmo *.cmi prog
