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

no_triplets(List) :-
    length(List, L),
    L < 3.

no_triplets([X, Y, Z | Rem]) :-
    X + Y + Z #> 0,
    X + Y + Z #< 3,
    no_triplets([Y, Z | Rem]).

all_distinct_lists(Lists) :-
    select(L1, Lists, OtherLists),
    member(L2, OtherLists),
    distinct_lists(L1, L2).

distinct_lists([], []).

distinct_lists([V1 | L1], [V2 | L2]) :-
    V1 #\= V2,
    distinct_lists(L1, L2).

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
