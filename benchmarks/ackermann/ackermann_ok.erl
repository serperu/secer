-module(ackermann_ok).
-export([main/1]).
-spec main([pos_integer()]) -> any(). 
main( [A, B] ) ->
  ack(A,B).

ack(0,N) -> N + 1;
ack(M,0) -> ack(M-1, 1);
ack(M,N) -> ack(M-1,ack(M,N-1)).
