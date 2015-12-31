/*facts*/
likes(dan, sally).
likes(sally, dan).
likes(josh, brittney).

/*rules*/
dating(X, Y) :-
likes(X, Y),
likes(Y, X).

friendship(X, Y) :-
likes(X, Y);
likes(Y, X).