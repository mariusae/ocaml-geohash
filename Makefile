all:
	$(MAKE) -C lib/

install: all
	$(MAKE) -C lib/ libinstall

clean:
	$(MAKE) -C lib clean
	$(MAKE) -f Makefile.gh clean

uninstall: all
	ocamlfind remove geohash

reinstall:
	$(MAKE) uninstall
	$(MAKE) install
