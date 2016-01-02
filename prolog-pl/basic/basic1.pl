/*facts*/
likes(dan, sally).
likes(sally, dan).
likes(josh, brittney).

/*rules*/
/*variable should be capitalized*/
dating(X, Y) :-
likes(X, Y),
likes(Y, X).

friendship(X, Y) :-
likes(X, Y);
likes(Y, X).

/*
If  -> :-
And -> ,
OR  -> ;
NOT -> not
*/

/*
['absolute file path'].
likes(dan, sally). => true.
likes(black, josh). => false.
dating(dan, sally). => true.
dating(josh, brittney). => false.
friendship(josh, brittney). => true .
*/