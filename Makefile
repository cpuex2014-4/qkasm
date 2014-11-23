SOURCES = \
	disasm.ml \
	statement.mli statement.ml parser.mly lexer.mll main.ml
RESULT = qkasm

OCAMLYACC = menhir
LIBS = nums

all: native-code


-include OCamlMakefile


