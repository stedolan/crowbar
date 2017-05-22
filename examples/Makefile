TESTS = calendar xmldiff pprint map fpath join opamver yojson cbor bson uunf bigint bencode bes
PKG_calendar = calendar
PKG_pprint = pprint,str
PKG_xmldiff = xmldiff
PKG_map =
PKG_fpath = fpath
PKG_join =
PKG_opamver = opam-core
PKG_yojson = yojson
PKG_cbor = cbor
PKG_bson = bson
PKG_uunf = uunf
PKG_bigint = num
PKG_bencode = bencode
PKG_bes = 

.PHONY: all clean
all: $(TESTS) alltests
clean:
	rm -f $(TESTS) alltests

$(TESTS): %: test_%.ml
	ocamlfind ocamlopt -package crowbar,$(PKG_$*) -linkpkg $^ -o $@ 

ALL_PKG = $(foreach v,$(TESTS),$(PKG_$(v)))
alltests: $(TESTS:%=test_%.ml)
	ocamlfind ocamlopt -package crowbar \
	  $(ALL_PKG:%=-package %) \
	  -linkpkg \
	  $^ -o $@

.gitignore: Makefile
	{ \
	   echo '*.cm*'; \
	   echo '*.o'; \
	   echo output; \
	   for i in $(TESTS) alltests; do echo $$i; done \
	} > $@


# HARD = pprint map fpath opamver uunf bigint bencode
HARD = pprint map fpath opamver bigint
hard: $(HARD:%=test_%.ml)
	ocamlfind ocamlopt -package crowbar \
	  $(ALL_PKG:%=-package %) \
	  -linkpkg \
	  $^ -o $@


