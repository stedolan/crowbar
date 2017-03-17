TESTS = calendar xmldiff pprint map
PKG_calendar = calendar
PKG_pprint = pprint,str
PKG_xmldiff = xmldiff
PKG_map =

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

