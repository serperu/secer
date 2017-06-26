ROOT_DIR = $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
ERLC_DIR = $(shell which erlc)
ERLC_PATH = $(shell dirname $(lastword $(ERLC_DIR)))

compile:
	@chmod +x secer
	@rm -Rf ebin
	@mkdir ebin
	@erlc -W0 -o ebin src/*.erl 
#	@mkdir tmp
#	@mkdir results
#	@cd proper; make
#	@cd cuter; autoconfig; ./configure; make depend; make

clean:
	@rm -Rf ebin
	@rm -Rf tmp

#run1:
#	@erl -pa ebin cuter/ebin proper/ebin -run secer run b1.erl 22 C 1 b1_slice.erl 18 C 1 numbers/2 15 -noshell -s erlang halt

#run2:
#	@erl -pa ebin cuter/ebin proper/ebin -run secer run string0.erl 225 Res 1 string1.erl 236 Res 1 tokens/2 15 -noshell -s erlang halt
#run2Error:
#	@erl -pa ebin cuter/ebin proper/ebin -run secer run string0.erl 225 Res 1 string2.erl 236 Res 1 tokens/2 15 -noshell -s erlang halt

#run3:
#	@erl -pa ebin cuter/ebin proper/ebin -run secer run happy0.erl 26 Happy 1 happy1.erl 11 Happy 1 main 15 -noshell -s erlang halt

#run4:
#	@erl -pa ebin cuter/ebin proper/ebin -run secer run prueba.erl 63 A 1 happy1.erl 11 Happy 1 main6/3 15 -noshell -s erlang halt	
