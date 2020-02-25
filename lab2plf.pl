%14
% a. Define a predicate to determine the longest sequences of consecutive
% even numb%ers (if exist more maximal sequences one of them).
% n_append(l1..ln,e) = [e], n = 0
%		    l1 + n_append(l2..ln), n > 0
% n_append(L:list, e:Element, R:list)
%n_append(i, i, o)

n_append([], E, R) :-
	R = [E].
n_append([H|T], E, [H|R]) :-
	n_append(T, E, R).

%listLength(l1..ln) = 0, n = 0
%		      1 + listLength(l2..ln), n > 0
%listLength(L:list, R:number)
%listLength(i, o)
listLength([], 0).
listLength([_|T], R) :-
	listLength(T, R1),
	R is R1 + 1.
%longestSeq(l1..ln, a1..am, r1..rl) = r1..rl, n = 0 && l >= m
%				      a1..am, n = 0 && l < m
%				      longestSeq(l2..ln, a1..am, l1 U r1..rl),
%                                                                    l1 % 2 = 0
%				      longestSeq(l2..ln, r1..rl, []), l1 % 2 = 1 && %                                                                  l >= m
%                                     longestSeq(l2..ln, a1..am, []), l1 % 2 = 1 &&
%                                                                  l < m
%longestSeq(L:list, A:list, AUX:list, R:list)
%longestSeq(i, i, i, o)
longestSeq([], A, AUX, R) :- listLength(A, L1),
           listLength(AUX, L2),
           L1 >= L2,
           R = A.
longestSeq([], A, AUX, R) :- listLength(A, L1),
           listLength(AUX, L2),
           L1 < L2,
           R = AUX.
longestSeq([H|T], A, AUX, R) :- H mod 2 =:= 0,
	   n_append(AUX, H, R1),
           longestSeq(T, A, R1, R).
longestSeq([_|T], A, AUX, R) :- listLength(A, L1),
           listLength(AUX, L2),
	   L1 >= L2,
           longestSeq(T, A, [], R).
longestSeq([_|T], A, AUX, R) :- listLength(A, L1),
           listLength(AUX, L2),
	   L1 < L2,
	   longestSeq(T, AUX, [], R).

%b
% For a heterogeneous list, formed from integer numbers and list of numbers, defin%e a predicate to replace
%every sublist with the longest sequences of even numbers from that sublist.
%replace(l1..ln) = longestSeq(l1, [], []) U replace(l2..ln), is_list(l1) = true,
%                  l1 U replace(l2..ln)
%replace(L:list, R:list).
%replace(i, o).
replace([], []).
replace([H|T], [HR|R]) :- is_list(H),
        longestSeq(H, [], [], HR),
	replace(T, R).
replace([H|T], [H|R]) :-
        replace(T, R).
