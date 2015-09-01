/** http://www.swi-prolog.org/pldoc/doc/swi/library/lists.pl */
:- use_module(library(lists)).

mover(L,R,B,S) :-
	append(L,B,L1),
	member(X,L),
	delete(L1,X,L2),
	not(incompatible(L2)),
	append([X],S2,S),
	movel(L2,R,[X],S2).

movel([],_,_,[]).
movel(L,R,B,S) :-
	append(R,B,R1),
	(incompatible(R1) -> return_load(L,R,R1,S); empty_return(L,R1,S)).

empty_return(L,R,S) :-
	append([empty],S2,S),
	mover(L,R,[],S2).

return_load(L,R,R1,S) :-
	member(Y,R),
	delete(R1,Y,R2),
	not(incompatible(R2)),
	append([Y],S2,S),
	mover(L,R2,[Y],S2).

incompatible([salad,goat]). 
incompatible([goat,salad]). 
incompatible([wolf,goat]). 
incompatible([goat,wolf]). 
