% prolog database 

likes(wallace, cheese).
likes(grommit, cheese).
likes(wendolene, sheep).

q1 :- likes(wallace, cheese).

friend(X, Y) :- \+(X = Y), likes(X, Z), likes(Y, Z).
