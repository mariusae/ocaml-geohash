all:
	$(MAKE) -C lib/

install: all
	$(MAKE) -C lib/ libinstall

uninstall: all
	ocamlfind remove geohash

reinstall:
	$(MAKE) uninstall
	$(MAKE) install
