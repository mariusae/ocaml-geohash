makelib:
	$(MAKE) -C lib/

all: reinstall
	$(MAKE) -f Makefile.gh
	$(MAKE) -C lib_test
	@echo ""

install: makelib
	$(MAKE) -C lib/ libinstall

clean:
	$(MAKE) -C lib clean
	$(MAKE) -C lib_test clean
	$(MAKE) -f Makefile.gh clean

uninstall: makelib
	ocamlfind remove geohash

reinstall:
	$(MAKE) uninstall
	$(MAKE) install
