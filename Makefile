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

run1:
	erl -pa ebin cuter/ebin proper/ebin -run secer_input_gen main b1.erl numbers 1021 1022 -noshell -s erlang halt

run2:
	erl -pa cuter/ebin -run b1_fake numbers b1 numbers 1 2 -noshell -s erlang halt

run3:
	erl -pa ebin cuter/ebin -run b1_fake numbers2 -noshell -s erlang halt

# run4:
# 	erl -pa ebin csp_tracker/ebin -run csp_reversible run examples/ex4.csp -noshell -s erlang halt

# run5:
# 	erl -pa ebin csp_tracker/ebin -run csp_reversible run examples/ex5.csp -noshell -s erlang halt

# run6:
# 	erl -pa ebin csp_tracker/ebin -run csp_reversible run examples/ex6.csp -noshell -s erlang halt

# run7:
# 	erl -pa ebin csp_tracker/ebin -run csp_reversible run examples/ex7.csp -noshell -s erlang halt

# run8:
# 	erl -pa ebin csp_tracker/ebin -run csp_reversible run examples/ex8.csp -noshell -s erlang halt

# update_sm:
# 	@cd csp_tracker; git checkout master && git pull
# 	@git add csp_tracker
# 	@git commit -m "updating csp_tracker to latest"
# 	@git push