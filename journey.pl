/* FACTS */

route(dublin, cork, 200, 'fct').
route(cork, dublin, 200, 'fct').
route(cork, corkAirport, 20, 'fc').
route(corkAirport, cork, 20, 'fc').
route(dublin, dublinAirport, 10, 'fc').
route(dublinAirport, dublin, 20, 'fc').
route(dublinAirport, corkAirport, 225, 'p').
route(corkAirport, dublinAirport, 225, 'p').

journey(S,S,_) :-
	write("You are already at this location.\nTime: 0 Hours").
journey(S,D,M) :-
	nb_setval(global1,[ ]),
	nb_setval(global3, [ ]),
	forall(route(S2,D2,Dist,Trans), (not(D2 == S), all_routes(M,S2,D2,Dist,Trans));true),
	nb_getval(global1,L),
	nb_setval(global2,L),
	locations(S,D),
	!.

all_routes(M,S,D,Dist,Trans) :- 
	atom_chars(M, N),
	atom_chars(Trans, Y),
	valid_modes(N,[ ],Valid),
	transport(Valid,Y,Dist, Speed),
	nb_getval(global3, List),
	list_empty(List, false) ->
	nb_getval(global1, L),
	append([[S,D,Speed]],L,L2),
	nb_setval(global1, L2),
	nb_setval(global3, [ ]).

valid_modes([ ],L,Valid):- Valid = L.
valid_modes([H|T],L,Valid) :-
	(H == p; H == t; H == c; H == f),
	append([H],L,L2),
	valid_modes(T,L2,Valid).

transport([ ], [_|_],_,_).
transport([H|_],Trans,Dist,Speed) :- 
	contains(H, Trans),
	calculations(H,Dist,Speed).
transport([_|T],Trans,Dist,Speed) :-
	transport(T,Trans,Dist,Speed).

contains(X,[X|_]) :-
	nb_getval(global3, L),
	append([X], L, L2),
	nb_setval(global3, L2).
contains(X,[_|T]) :- 
	contains(X,T).

calculations(p,Y,Speed) :- Speed is Y / 500.
calculations(t,Y,Speed) :- Speed is Y / 100.
calculations(c,Y,Speed) :- Speed is Y / 80.
calculations(f,Y,Speed) :- Speed is Y / 5.

locations(S,D) :-
	nb_setval(global1, []),
	nb_getval(global2, Legal),
	travel(S,D,Legal,NewLegal),
	nb_getval(global1, Route_list),
	nb_getval(global2, Score),
	append([Score],Route_list,List),
	append([List],[],Finale),
	nb_setval(global1, []),
	travel_cycle(S,D,NewLegal,_,Finale).

travel_cycle(S,D,Legal,NewLegal,L) :-
	(travel(S,D,Legal,NewLegal),
	nb_getval(global1, Route_list),
	nb_getval(global2, Score),
	append([Score],Route_list,List),
	append([List],L,Finale),
	nb_setval(global1, []),
	travel_cycle(S,D,NewLegal,_,Finale));
	(shortest(L)).

travel(S,D,Legal,NewLegal) :- route(S,D,_,_), legality(S,D,Legal,X), nb_setval(global1, [S,D]), nb_setval(global2, X), delete(Legal,[S,D,X],NewLegal).
travel(S,D,Legal,NewLegal) :-
	route(S,Z,_,_),
	legality(S,Z,Legal,Time) ->
	delete(Legal, [S,Z,Time], Legal2),
	(travel(Z,D,Legal2,NewLegal),
	nb_getval(global1, Route_list),
	append([S],Route_list,Appended_route_list),
	nb_setval(global1, Appended_route_list),
	nb_getval(global2, Score2),
	Score is (Score2 + Time),
	nb_setval(global2, Score)).

legality(X,Y,[H|_],Z) :-
	head(S2,H),
	tail(T,H),
	head(D2,T),
	ret_last(H,Z),
	X == S2, 
	Y == D2.
legality(X,Y,[_|T],Z) :- legality(X,Y,T,Z).

shortest(Finale) :-
	nb_setval(global1, []),
	num_append(Finale),
	nb_getval(global1, Numbers),
	list_min(Numbers, Y),
	endgame(Y,Finale).

num_append([ ]).
num_append([H|T]) :- num_append(T), head(H2,H), nb_getval(global1, Numbers),append([H2],Numbers, L2), nb_setval(global1, L2).

list_min([L|T], Min) :- list_min(T, L, Min).
list_min([ ], Min, Min).
list_min([L|T], Min0, Min) :-
    Min1 is min(L, Min0),
    list_min(T, Min1, Min).

endgame(Min,[H|_]):-
	head(H2,H),
	Min == H2,
	delete(H,Min,Route),
	atomic_list_concat(Route,'-',S),
	atom_number(N, Min),
	write("Fastest route: "),
	writeln(S),
	write("Time: "),
	write(N),
	write(" Hours.").
endgame(Min,[_|T]):- endgame(Min,T).

list_empty([ ], true).
list_empty([_|_], false).

ret_last([X],X).
ret_last([_|T],X) :- ret_last(T,X).

head(H,[H|_]).
tail(T,[_|T]).
