:- use_module(library(clpfd)).
:- use_module(library(lists)).

binary_puzzle(Rows) :-
    % Check validity of board
    length(Rows, L),
    mod(L, 2) =:= 0,
    maplist(same_length(Rows), Rows),
    append(Rows, Vs), Vs ins 0..1,
    % Check rows
    maplist(valid_binary_puzzle_row, Rows),
    all_distinct_lists(Rows),
    % Check columns
    transpose(Rows, Cols),
    maplist(valid_binary_puzzle_row, Cols),
    all_distinct_lists(Cols).

valid_binary_puzzle_row(Row) :-
    length(Row, L),
    HL is L // 2,
    global_cardinality(Row, [0-HL, 1-HL]),
    no_triplets(Row).

no_triplets([X, Y, Z | Rem]) :-
    X + Y + Z #> 0,
    X + Y + Z #< 3,
    no_triplets([Y, Z | Rem]).

no_triplets(List) :-
    length(List, L),
    L < 3.

all_combinations(E, [L|List], [[E, L]|Out]) :-
    all_combinations(E, List, Out).

all_combinations(_E, [], []).

unique_combinations([L|List], Out) :-
    all_combinations(L, List, First),
    unique_combinations(List, Second),
    append([First, Second], Out).

unique_combinations([], []).

all_distinct_lists(Lists) :-
    unique_combinations(Lists, ListPairs),
    maplist(apply(distinct_lists), ListPairs).

distinct_lists(L1, L2) :-
    list_to_num(L1, N1),
    list_to_num(L2, N2),
    N1 #\= N2.

list_to_num(List, Num) :-
    list_to_num_aux(List, 1, Num).

list_to_num_aux([E|List], N, Num) :-
    N2 is N * 2,
    list_to_num_aux(List, N2, NumP),
    Num #= E*N + NumP.

list_to_num_aux([], N, N).

print_binary_puzzle_solution(Rows) :-
    binary_puzzle(Rows),
    maplist(labeling([ff]), Rows),
    !,
    maplist(portray_clause, Rows).

binary_puzzle_problem(1, [[_, 1, _, _, _, _, _, _, _, _],
                          [_, 1, 1, _, _, _, _, _, _, 1],
                          [_, _, 1, _, _, 0, _, _, 0, _],
                          [_, _, _, _, _, 0, _, _, _, _],
                          [_, _, _, _, _, _, _, 1, 1, _],
                          [_, _, _, 0, _, _, _, _, _, _],
                          [_, _, _, _, _, _, 1, _, _, _],
                          [1, 1, _, _, 0, 0, _, _, _, _],
                          [_, _, _, _, _, 0, _, _, _, _],
                          [1, _, _, _, _, _, _, _, 1, _]]).
