LANG := en_US.UTF-8
dist:
	$(MAKE) -C libzauth-c $@
	cp libzauth-c/target/release/*.deb .

clean:
	$(MAKE) -C libzauth-c $@
	rm -f *.deb

.DEFAULT:
	$(MAKE) -C libzauth-c $@
