:- begin_tests(sa4_tests).

:- consult([friends, family, sa4]). % Load the necessary Prolog files


% Tests for grandparent/2
test(grandparent) :-
    once(grandparent(georgeI, georgeII)),
    \+ grandparent(dio, jotaro).

% Tests for siblings/2
test(siblings) :-
    once(siblings(josuke, holy)),
    \+ siblings(jonathan, giorno),
    \+ siblings(jonathan, jonathan).

% Tests for auntOrUncle/2
test(auntOrUncle) :-
    once(auntOrUncle(josuke, jotaro)),
    \+ auntOrUncle(dio, josuke).

% Tests for ancestor/2
test(ancestor) :-
    once(ancestor(georgeI, joseph)),
    once(ancestor(dio, donatello)),
    \+ ancestor(jotaro, dio).

% Tests for friends/2
test(friends) :-
    once(friends(alan, bichuan)),
    once(friends(bichuan, alan)),
    \+ friends(alan, lucas),
    \+ friends(lucas, lucas).

% Tests for commonFriend/3
test(commonFriend) :-
    once(commonFriend(alan, elena, maria)),
    \+ commonFriend(alan, deshawn, coco).

% Tests for suggestFriend/2
test(suggestFriend) :-
    once(suggestFriend(alan, zahra)),
    \+ suggestFriend(ali, elena).

% Tests for suggestFollow/2
test(suggestFollow) :-
    once(suggestFollow(lucas, alan)),
    \+ suggestFollow(deshawn, coco).

% Tests for fibonacci/2
test(fibonacci) :-
    once(fibonacci(5, 5)),
    once(fibonacci(7, 13)),
    \+ fibonacci(6, 20).

% Tests for isSet/1
test(isSet) :-
    once(isSet([1,2,3,4])),
    \+ isSet([1,2,2,3]).

% Tests for dupList/2
test(dupList) :-
    once(dupList([1,2,3], [1,1,2,2,3,3])).

% Tests for removeDuplicates/2
test(removeDuplicates) :-
    once(removeDuplicates([1,2,2,3,3,4], [1,2,3,4])).

% Tests for everyOtherOne/2
test(everyOtherOne) :-
    once(everyOtherOne([1,2,3,4,5], [1,3,5])).

% Tests for setEq/2
test(setEq) :-
    once(setEq([1,2,3], [3,2,1])).

% Tests for maxList/2
test(maxList) :-
    once(maxList([3,5,2,8,7], 8)).


:- end_tests(sa4_tests).
:- set_prolog_flag(verbose, true).
:- run_tests.
:- halt.
