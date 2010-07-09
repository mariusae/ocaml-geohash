OCAMLMAKEFILE=../OCamlMakefile

ANNOTATE = yes
SOURCES = geohash.ml
RESULT = geohash
#LIB_PACK_NAME = geohash
LIBINSTALL_FILES = META geohash.cma geohash.cmxa geohash.a geohash.cmi
PACKS = batteries
FLAGS = -thread

.PHONY: all
all: ncl dcl
	@ :

include $(OCAMLMAKEFILE)