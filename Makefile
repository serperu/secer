ROOT_DIR = $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
ERLC_DIR = $(shell which erlc)
ERLC_PATH = $(shell dirname $(lastword $(ERLC_DIR)))

compile:
	@rm -Rf ebin
	@mkdir ebin
	@erlc -W0 -o ebin src/*.erl 
#	@cd proper; make
#	@cd cuter; autoconfig; ./configure; make depend; make

clean:
	@rm -Rf ebin

run1:
	@erl -pa ebin cuter/ebin proper/ebin -run secer run b1.erl 22 C 1 b1_slice.erl 18 C 1 numbers 15 -noshell -s erlang halt

run2:
	@erl -pa ebin cuter/ebin proper/ebin -run secer run string0.erl 5948 5951 string1.erl 6138 6141 tokens 15 -noshell -s erlang halt

run3:
	@erl -pa ebin cuter/ebin proper/ebin -run secer run happy0.erl 425 430 happy1.erl 222 227 main 15 -noshell -s erlang halt
#	@erl -pa ebin cuter/ebin proper/ebin -run secer run b1.erl 1022 1023 numbers 15 -noshell -s erlang halt
	
