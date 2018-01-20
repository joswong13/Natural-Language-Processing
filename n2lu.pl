/* Issam Fadel 500692771 Section 5
Joseph Wong 500304948 Section 4*/


male(john). male(kevin). male(steve). male(frank). male(omar). male(tom). male(mark). male(nathan).
 male(dave). male(zack). male(logan). male(manny).
 
female(alice). female(sarah). female(sam). female(linda). female(jane). female(becky). female(carol).
female(helen). female(debrah).

person(john). person(kevin). person(steve). person(frank). person(omar). person(becky). person(nathan).
person(alice). person(sarah). person(linda). person(sam). person(jane). person(carol). person(mark).
person(dave). person(helen). person(zack). person(logan). person(debrah). person(manny).

home(john, brampton). home(kevin, toronto). home(steve, mumbai). home(frank, beirut). home(omar, etobicoke).
home(alice, kitchener). home(sarah, hongkong). home(linda, ottawa). home(sam, tokyo). home(jane, sydney).
home(becky, london). home(nathan, losAngeles). home(carol, montreal). home(mark, montreal). home(tom, ottawa).
home(dave, toronto). home(helen, toronto). home(zack, toronto). home(logan, seattle). home(debrah, losAngeles).
home(manny, losAngeles).

city(brampton). city(toronto). city(mumbai). city(beirut). city(etobicoke). city(montreal). city(london).
city(kitchener). city(hongkong). city(ottawa). city(tokyo). city(sydney). city(losAngeles). city(seattle).

country(canada). country(india). country(lebanon). country(china). country(japan).
country(australia). country(america). country(england).

location(brampton, canada). location(toronto, canada). location(mumbai, india). location(beirut, lebanon). location(etobicoke, canada).
location(kitchener, canada). location(hongkong, china). location(ottawa, canada). location(tokyo, japan). location(sydney, australia).
location(london, england). location(losAngeles, america). location(montreal, canada). location(seattle, america).

population(brampton, small). population(toronto, biggest). population(mumbai, big). population(beirut, small). population(etobicoke, big).
population(kitchener, smallest). population(hongkong, big). population(ottawa, small). population(tokyo, big). population(sydney, big).
population(london, biggest). population(montreal, biggest). population(losAngeles, biggest). population(seattle, big).

married(john, alice). married(frank, sam). married(kevin, sarah). married(tom, linda). married(mark, carol).
married(alice, john). married(sam, frank). married(sarah, kevin). married(linda, tom). married(carol, mark).
married(helen,dave). married(dave,helen). married(debrah, manny). married(manny, debrah).

parent(mark,debrah). parent(carol, debrah).
parent(john, frank). parent(alice, frank).
parent(dave, john). parent(helen, john).
parent(john, kevin). parent(alice, kevin).
parent(john, becky). parent(alice, becky).

parent(frank, omar). parent(sam, omar).

parent(kevin, steve). parent(sarah, steve).

parent(kevin, jane). parent(sarah, jane).

parent(tom, alice). parent(linda, alice).

parent(carol, nathan). parent(mark, nathan).


friend(john, kevin). friend(steve, frank). friend(alice, linda). friend(sarah, jane). friend(becky, nathan). friend(steve, kevin).
friend(kevin, john). friend(frank, steve). friend(linda, alice). friend(jane, sarah). friend(nathan, becky). friend(steve, kevin).
friend(zack, nathan). friend(nathan, zack). friend(zack, logan). friend(logan, zack).

father(X, Y):- male(X), parent(X, Y).

mother(X, Y):- female(X), parent(X, Y).

brother(X, Y):-  not(X = Y), male(X), parent(P1, X), parent(P1, Y).

sister(X,Y):- female(X), parent(P1, X), parent(P1, Y),not(X=Y).

ancestor(A, B) :- parent(A, B).
ancestor(A, B) :- parent(A, X), ancestor(X, B).

grandmother(X,Y):- female(X), parent(X, P1), parent(P1, Y).
grandfather(X,Y):- male(X), parent(X,P1), parent(P1,Y).

uncle(X,Y):- brother(X,Sib), parent(Sib,Y).
auntie(X,Y):- sister(X,Sib), parent(Sib,Y).

relative(X, Y):- brother(X, Y).
relative(X, Y):- sister(X, Y).
relative(X, Y):- ancestor(X, Y). 
relative(X, Y):- uncle(X, Y).
relative(X, Y):- auntie(X, Y).
relative(X, Y):- parent(A, X), parent(B, Y), sister(A, B).
relative(X, Y):- parent(A, X), parent(B, Y), brother(A, B).
relative(X, Y):- parent(A, X), parent(B, Y), relative(A, B).

in(X, Y):- location(X, Y).

article(a).
article(an).
article(any).
article(the).

common_noun(man, X):- male(X).
common_noun(woman, X):- female(X).
common_noun(father, X):- father(X, Y).
common_noun(mother, X):- mother(X, Y).
common_noun(wife, X):- married(X, Y), female(X).
common_noun(husband, X):- married(X, Y), male(X).
common_noun(child, X):- parent(Y, X).
common_noun(grandmother, X):- grandmother(X, Y).
common_noun(grandfather, X):- grandfather(X, Y).
common_noun(brother, X):- brother(X, Y).
common_noun(sister, X):- sister(X, Y).
common_noun(uncle, X):- uncle(X, Y).
common_noun(auntie, X):- auntie(X, Y).
common_noun(parent, X):- parent(X, Y).
common_noun(ancestor, X):- ancestor(X, Y).
common_noun(relative, X):- relative(X, Y).
common_noun(city, X):- city(X).
common_noun(country, X):- country(X).
common_noun(friend, X):- friend(X, Y).
common_noun(person, X):- person(X).

preposition(in, X, Y):- location(X, Y).
preposition(in, X, Y):- home(X, Y).
preposition(from, X, Y):- home(X, Y).
preposition(from, X, Y):- home(X, Z), location(Z,Y).
preposition(of, X, Y):- home(X, Y).
preposition(of, X, Y):- location(X, Y).
preposition(of, X, Y):- friend(X, Y).
preposition(with, X, Y):- married(X,Y).
preposition(with, X, Y):- friend(X,Y).
preposition(with, X, Y):- parent(X,Y).

adjective(small, X):-population(X, small).
adjective(large, X):-population(X, big).
adjective(smallest, X):-population(X, smallest).
adjective(biggest, X):-population(X, biggest).
adjective(largest, X):-population(X, biggest).
adjective(married, X):- married(X, Y).
adjective(single, X):- person(X), not(married(X, Y)).
adjective(american, X):- home(X, Y), location(Y, america).
adjective(canadian, X):- home(X, Y), location(Y, canada).
adjective(chinese, X):- home(X, Y), location(Y, china).

proper_noun(X) :- city(X).
proper_noun(X) :- person(X).
proper_noun(X) :- country(X).

/******************* parser **********************/

who(Words, Ref) :- np(Words, Ref).
what(Words, Ref) :- np(Words, Ref).
/* Noun phrase can be a proper name or can start with an article */

np([Name],Name) :- proper_noun(Name).
np([Art|Rest], Who) :- article(Art), np2(Rest, Who).


/* If a noun phrase starts with an article, then it must be followed
   by another noun phrase that starts either with an adjective
   or with a common noun. */

np2([Adj|Rest],Who) :- adjective(Adj,Who), np2(Rest, Who).
np2([Noun|Rest], Who) :- common_noun(Noun, Who), mods(Rest,Who).


/* Modifier(s) provide an additional specific info about nouns.
   Modifier can be a prepositional phrase followed by none, one or more
   additional modifiers.  */

mods([], _).
mods(Words, Who) :-
	appendLists(Start, End, Words),
	prepPhrase(Start, Who),	mods(End, Who).

prepPhrase([Prep|Rest], Who) :-
	preposition(Prep, Who, Ref), np(Rest, Ref).

appendLists([], L, L).
appendLists([H|L1], L2, [H|L3]) :-  appendLists(L1, L2, L3).
