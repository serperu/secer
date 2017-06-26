This is a modified version of CutEr: a concolic unit testing tool for Erlang.

Copyright 2014-2017 by Aggelos Giantsios, Nikolaos Papaspyrou and Kostis Sagonas.

The original tool is publicly available at: https://github.com/aggelgian/cuter

We performed an adaptation of some of its modules in order to use some powerful parts of the tool in our work. 
The modified modules and functions are the following:

cuter_symbolic.erl:
- generate_new_input/2

cuter_pp.erl:
- pp_nl/2
- pp_solving_failure/2
- pp_execution_status_minimal/1

cuter.erl:
- start/2


