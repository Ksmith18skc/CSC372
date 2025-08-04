/*
Solutions for SA4 Assignment, Intro to Prolog

Name: Kory Smith
Time spent on SA4: ~2 hours

Collaborators and references: Prolog documentation, and the course slides.

*/


% grandparent(X, Y): X is a grandparent of Y.
grandparent(X, Y) :- parent(X, Z), parent(Z, Y).
% grandparent(_,_) :- fail.

% siblings(X, Y): X and Y are siblings. True if X and Y share at least one common parent.
siblings(X, Y) :- parent(P, X), parent(P, Y), X \= Y.

% auntOrUncle(X, Y): X is either an aunt or uncle of Y.
auntOrUncle(X, Y) :- siblings(X, Z), parent(Z, Y).

% ancestor(X, Y): X is an ancestor of Y.
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).


% friends(X, Y): X and Y are friends, makes the friendship reciprocal.
friends(X, Y) :- friend(X, Y).
friends(X, Y) :- friend(Y, X).

% commonFriend(X, Y, Z): Z is a friend of both X and Y.
commonFriend(X, Y, Z) :- friends(X, Z), friends(Y, Z).

% suggestFriend(X,Y): Suggest X and Y be friends if they have a friend in common and are not already friends, or if they
% follow the same person and are not already friends.
suggestFriend(X, Y) :- commonFriend(X, Y, Z), \+ friends(X, Y).
suggestFriend(X, Y) :- follows(X, Z), follows(Y, Z), \+ friends(X, Y).

% suggestFollow(X,Y): Suggest that X follow Y if:
% - X follows someone who follows Y and X does not already follow Y,
% - or X follows someone who is friends with Y and X does not already follow Y,
% - or X is friends with someone who follows Y and X does not already follow Y.
suggestFollow(X, Y) :- follows(X, Z), follows(Z, Y), \+ follows(X, Y).
suggestFollow(X, Y) :- follows(X, Z), friends(Z, Y), \+ follows(X, Y).
suggestFollow(X, Y) :- friends(X, Z), follows(Z, Y), \+ follows(X, Y).


% fibonacci(N, F): F is the Nth fibonacci number.
fibonacci(0, 0).
fibonacci(1, 1).
fibonacci(N, F) :- N > 1, N1 is N - 1, N2 is N - 2, fibonacci(N1, F1), fibonacci(N2, F2), F is F1 + F2.

% isSet(X): X is a list with unique elements.
isSet([]).
isSet([H|T]) :- \+ member(H, T), isSet(T).

% dupList(X, Y): Y is a List X with each element repeated once.
dupList([], []).
dupList([H|T], [H,H|T2]) :- dupList(T, T2).

% removeDuplicates(X, Y): Y is the list obtained by removing duplicate elements from X.
removeDuplicates([], []).
removeDuplicates([H|T], [H|T2]) :- \+ member(H, T), removeDuplicates(T, T2).
removeDuplicates([H|T], T2) :- member(H, T), removeDuplicates(T, T2).

% everyOtherOne(X, Y): Y is a list that contains every other element from list X.
everyOtherOne([], []).
everyOtherOne([X], [X]).
everyOtherOne([X,_|T], [X|T2]) :- everyOtherOne(T, T2).

% setEq(X, Y): X and Y contain the same elements, regardless of order.
setEq(X, Y) :- sort(X, SX), sort(Y, SY), SX = SY.

% maxList(L, Max): Max is the largest element in the list L.
maxList([H], H).
maxList([H|T], Max) :- maxList(T, MaxT), Max is max(H, MaxT).