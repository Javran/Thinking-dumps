% init
:- initialization(['common.pl']).
:- initialization(queryAll).

% facts

food_type(velveeta, cheese).
food_type(ritz, cracker).
food_type(spam, meat).
food_type(sausage, meat).
food_type(jolt, soda).
food_type(twinkie, dessert).

flavor(sweet, dessert).
flavor(savory, meat).
flavor(savory, cheese).
flavor(sweet, soda).

food_flavor(X, Y) :- food_type(X, Z), flavor(Y, Z).

% queries

query(food_type(What, meat)).
% spam
query(findall(X, food_type(X, meat), Y)).
% spam and sausage
query(food_flavor(sausage, sweet)).
% food_type(sausage, meat) and flavor(savory, meat), so:
% no
query(food_flavor(sausage, savory)).
% this will be yes
query(findall(X, flavor(sweet, X), Y)).
% it's dessert and soda
query(findall(X, food_flavor(X, savory),Y)).
% food_flavor(X,Y) --> Y -> savory
% so meat and cheese can be filled to Z
% meat -> (spam, sausage) and cheese -> (velveeta)
% so the output is 'spam, sausage, velveeta'
