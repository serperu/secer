ROOT_DIR = $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
ERLC_DIR = $(shell which erlc)
ERLC_PATH = $(shell dirname $(lastword $(ERLC_DIR)))

compile:
	@chmod +x secer
	#@cd cuter; ./fetch_protoc.sh; git submodule init && git submodule update; git submodule foreach make; autoconf; ./configure --with-protoc=$(ROOT_DIR)/cuter/lib/protoc-3.2.0/bin/protoc; make depend; make
	@cd cuter/lib/proper; make
	@make csecer

csecer:
	@rm -Rf ebin
	@mkdir ebin
	@erlc -W0 -o ebin src/*.erl 
	@erlc -W0 -o ebin src/dependences/*.erl 
	@mkdir -p tmp
	@mkdir -p results
clean:
	@rm -Rf ebin
	@rm -Rf tmp
	@rm -Rf ./cuter/ebin
	@rm -Rf ./proper/ebin

run:
	./secer -f b1.erl -li 19 -var X -f b1_slice.erl -li 6 -var X -to 15
run2:
	./secer -f b1.erl -li 25 -var C -f b1_slice.erl -li 9 -var C -funs [numbers/2] -to 15