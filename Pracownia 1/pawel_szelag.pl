% Definiujemy moduł zawierający rozwiązanie.
% Należy zmienić nazwę modułu na {imie}_{nazwisko} gdzie za
% {imie} i {nazwisko} należy podstawić odpowiednio swoje imię
% i nazwisko bez znaków diakrytycznych
:- module(pawel_szelag, [solve/2]).
% definiujemy operatory ~/1 oraz v/2
:- op(200, fx, ~).
:- op(500, xfy, v).
% Główny predykat rozwiązujący zadanie.
% UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jego
% definicję.

solve(Clauses, Solution) :-
	create_list_of_clauses(Clauses,List),
	not(check_for_empty(List)),
	get_vars_from_list(List,Variables),
	get_perms(Variables,X),
	setof(X,check_list_of_clauses(List,X),[Solution]).


%czy jest literałem
lit(A) :- atom(A).
lit(~A) :- atom(A).

%jesli jest zanegowaną zmienną zwraca tę zmienną
is_neg(~A,A).

%sprawdza czy literał to zanegowana zmienna
check_neg(~_).

%zwraca pierwszy element klauzuli
get_first_el_of_clause(A,A) :- lit(A).
get_first_el_of_clause(A v _, A) :- lit(A).

%zwraca klauzule bez pierwszego elementu
get_rest_of_clause(A v B, B) :- lit(A).
get_rest_of_clause(_, []).

%przekształca klauzulę na listę literałów (np. p v q v r -> [p,q,r])
clause_to_list(Clause, List) :-
	clause_to_list(Clause, [], List).

clause_to_list([], List, List) :- !.

clause_to_list(H, T, Acc) :-
	get_first_el_of_clause(H,H1),
	get_rest_of_clause(H,T1),
	clause_to_list(T1, [H1|T], Acc),
	!.

%korzystając z poprzedniego predykatu przekształca wejście na listę list w powyższej postaci
create_list_of_clauses([],[]).
create_list_of_clauses([H|T], [H1|T1]) :-
	clause_to_list(H,H1),
	create_list_of_clauses(T,T1).

%sprawdza czy w liście klazul jest klazula pusta
check_for_empty([[]]).
check_for_empty([H|_]) :- H = [], !.
check_for_empty([_|T]) :- check_for_empty(T), !.

%szuka wartosciowania konkretnej zmiennej wśród listy wartosciowań w postaci np. [(p,t),(q,f),(r,t)]
look_for_val(X, [(A,V)|_], Res) :- 
	lit(X),
	X = A,
	Res = V,
	!.
look_for_val(X, [(_,_)|T], Res) :-
	look_for_val(X, T, Res).

%zwraca true jesli klauzula w postaci listy (np. [p,q,~r]) jest spełniona przy pomocy danych wartosciowań (Val_list)
is_alt_true([],t).

is_alt_true([H|T], Val_list) :-
	not(check_neg(H)),
	look_for_val(H, Val_list, Res),
	Res = t;
	is_alt_true(T, Val_list),
	!.
is_alt_true([H|T], Val_list) :-
	is_neg(H,A),
	look_for_val(A, Val_list, Res),
	Res = f;
	is_alt_true(T,Val_list),
	!.


is_alt_true([_|T], Val_list) :-
	T \= [],
	is_alt_true(T, Val_list).

%używa is_alt_true na elementach listy klauzul
check_list_of_clauses([],_).
check_list_of_clauses([H|T], Val_list) :- 
	is_alt_true(H, Val_list),
	check_list_of_clauses(T, Val_list).

%wyciąga z klauzuli zmienne (te zanegowane przekształca do postaci niezanegowanej), potrzebne to będzie przy sprawdzaniu jakie zmienne są w zbiorze klauzul. nie bierze pod uwagi pustych

 
get_vars_from_clause([H|T],List) :-
    get_vars_from_clause([H|T],[],List).
 
get_vars_from_clause([],List,List).
 
get_vars_from_clause([H|T], T1, Acc) :-
    H = [],
    get_vars_from_clause(T, T1, Acc),
    !.
 
get_vars_from_clause([H|T], T1, Acc) :-
    not(check_neg(H)),
    not(member(H,T1)),
    get_vars_from_clause(T, [H|T1], Acc),
    !.
 
get_vars_from_clause([H|T], T1, Acc) :-
    check_neg(H),
    not(member(H,T1)),
    is_neg(H,H1),
    get_vars_from_clause(T, [H1|T1], Acc),
    !.

get_vars_from_clause([H|T], T1, Acc) :-
    check_neg(H),
    member(H,T1),
    get_vars_from_clause(T, T1, Acc),
    !.

get_vars_from_clause([H|T], T1, Acc) :-
    not(check_neg(H)),
   	member(H,T1),
    get_vars_from_clause(T, T1, Acc),
    !.
 
%tworzy listę zmiennych użytych w zbiorze klauzul 
get_vars_from_list([],[]).
get_vars_from_list([H|T],List) :-
    get_vars_from_clause(H, List1),
    get_vars_from_list(T, List2), append(List2,List1,List3), setof(X,member(X,List3),List).

%zwraca permutacje wartosciowań dla danej listy zmiennych (np. dla [p,q,r] kolejno [(p,t),(q,t),(r,t)],[(p,t),(q,t),(r,f)]) itd.
get_perms([],[]).
get_perms([H|T], [H1|T1]) :- (H1 = (H,t); H1 = (H,f)), get_perms(T,T1).
