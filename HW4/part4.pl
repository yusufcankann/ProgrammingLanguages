% rules


%checks, does element E iosis member of S
element(E,S) :- member(E,S),!.



%checks all the permutation fo S1 and S2 if they are equvalent.
equivalent(S1, S2) :-
    permutation(S1, S2).



%if last elements are equal, the statement is true.
union([],S2,S2).

%check first element of S1 is a member of S2.
%If it is call union recursively.
union([E|S1],S2,S3):-
    element(E,S2),
    union(S1,S2,S3).

%if first element of S1 is not element of S2 or S3 pass
% first elements of s1 and s3 and call union recursively
union([E|S1],S2,[E|S3]) :-
    \+ element(E,S2),
    union(S1,S2,S3).





intersect(S1,S2,S3) :- calculateIntesect(S1,S2,E), equivalent(E,S3).

calculateIntesect([],_, []).
calculateIntesect([E|S1], S2, [E|S3]):- element(E, S2), !, intersect(S1, S2, S3).
calculateIntesect([_|S1], S2, S3):- intersect(S1, S2, S3).

