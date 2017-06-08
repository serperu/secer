ROOT_DIR = $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
ERLC_DIR = $(shell which erlc)
ERLC_PATH = $(shell dirname $(lastword $(ERLC_DIR)))

compile:
	@rm -Rf ebin
	@mkdir ebin
	@erlc -W0 -o ebin src/*.erl 
#	@cd proper; make
#	@cd cuter; make

clean:
	@rm -Rf ebin
#	@rm -f *.txt

# run_paper:
# 	erl -pa ebin csp_tracker/ebin -run csp_reversible run examples/ex_paper.csp -noshell -s erlang halt
# erl -pa ebin cuter/ebin proper/ebin -run secer run b1.erl 1021 1022 b1_slice.erl 712 713 numbers 15 -noshell -s erlang halt
run1:
	erl -pa ebin cuter/ebin proper/ebin -run secer run b1.erl 1021 1022 numbers 15 -noshell -s erlang halt

run2:
	erl -pa ebin cuter/ebin proper/ebin -run secer run b1_slice.erl 712 713 numbers 15 -noshell -s erlang halt

run3:
	erl -pa ebin cuter/ebin proper/ebin -run secer run b1.erl 1025 1026 b1_slice.erl 712 713 numbers 15 -noshell -s erlang halt


# update_sm:
# 	@cd csp_tracker; git checkout master && git pull
# 	@git add csp_tracker
# 	@git commit -m "updating csp_tracker to latest"
# 	@git push