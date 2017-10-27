ROOT_DIR = $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
ERLC_DIR = $(shell which erlc)
ERLC_PATH = $(shell dirname $(lastword $(ERLC_DIR)))

compile:
	@chmod +x secer
	@chmod +x ./config/makeCuter.sh	
	@mkdir -p tmp
	@touch ./config/nocuter.txt
	@cd cuter/lib/proper; make
	#@dialyzer --build_plt --apps erts kernel stdlib crypto mnesia sasl 
	@make csecer 
	@./config/makeCuter.sh $(ROOT_DIR)


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
