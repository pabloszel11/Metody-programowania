% Definiujemy moduł zawierający testy.
% Należy zmienić nazwę modułu na {imie}_{nazwisko}_tests gdzie za
% {imie} i {nazwisko} należy podstawić odpowiednio swoje imię
% i nazwisko bez znaków diakrytycznych
:- module(pawel_szelag_tests, [tests/5]).

% definiujemy operatory ~/1 oraz v/2
:- op(200, fx, ~).
:- op(500, xfy, v).

% Zbiór faktów definiujących testy

tests(excluded_middle, validity, [p v ~p], 500, solution([(p,t)])).
tests(single_variable, validity, [p], 500, solution([(p,t)])).
tests(denied_variable, validity, [p, ~p], 500, count(0)).
tests(empty_clauses, validity, [p, []], 500, count(0)).
tests(single_denied_variable, validity, [~p], 500, solution([(p,f)])).
tests(three_variables, validity, [p v q v r], 500, solution([(p,t),(q,t),(r,t)])).
tests(two_variables, validity, [p v q], 500, solution([(p,t),(q,t)])).
tests(single_empty_clause, validity, [[]], 500, count(0)).
tests(multiple_variable, validity, [p v q v p v p v q], 500, solution([(p,f),(q,t)])).
tests(multiple_single_variables, validity, [~p,q,~p,q,r,s,r,s], 500, solution([(p,f),(q,t),(r,t),(s,t)])).
tests(one_answer, validity, [~p, q v p, q, ~r v s, ~s, ~r], 500, solution([(p,f),(q,t),(r,f),(s,f)])).
tests(one_answer, validity, [~p, q v p, q, ~r v s, ~s, ~r], 500, count(1)).