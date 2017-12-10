reduce-algebra:
	rm -rf reduce-algebra
	svn co http://svn.code.sf.net/p/reduce-algebra/code/trunk reduce-algebra

reduce-algebra/bin/redcsl: reduce-algebra
	# May need to run sudo apt-get install libtool-bin
	rm reduce-algebra/bin/redcsl
	cd reduce-algebra && ./configure --with-csl --without-gui && make

test_reduce:
	cd reduce-algebra && scripts/testall.sh --csl
