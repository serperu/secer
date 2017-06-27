ROOT_DIR = $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
ERLC_DIR = $(shell which erlc)
ERLC_PATH = $(shell dirname $(lastword $(ERLC_DIR)))

compile:
	@chmod +x secer
	@rm -Rf ebin
	@mkdir ebin
	@erlc -W0 -o ebin src/*.erl 
	@mkdir -p tmp
	@mkdir -p results
	@cd proper; make
	@cd cuter; autoconf; ./configure; make depend; make

clean:
	@rm -Rf ebin
	@rm -Rf tmp
	@rm -Rf ./cuter/ebin
	@rm -Rf ./proper/ebin
