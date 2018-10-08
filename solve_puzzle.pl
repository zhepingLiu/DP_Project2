% Declarative Programming Project 2
% Author : Zheping Liu, 683781, zhepingl

:- ensure_loaded(proj2).
:- ensure_loaded(library(clpfd)).

% solve_puzzle(Puzzle0, WordList, Puzzle)
% should hold when Puzzle is a solved version of Puzzle0, with the
% empty slots filled in with words from WordList.  Puzzle0 and Puzzle
% should be lists of lists of characters (single-character atoms), one
% list per puzzle row.  WordList is also a list of lists of
% characters, one list per word.

solve_puzzle(Puzzle0, WordList, Puzzle) :-
    % The format of Puzzle1 is not really a table
    solve_puzzle_iteration(Puzzle0, WordList, WordList1, 1, Puzzle1),
    clpfd:transpose(Puzzle1, TransPuzzle1),
    solve_puzzle_iteration(TransPuzzle1, WordList1, [], 1, Puzzle2),
    clpfd:transpose(Puzzle2, Puzzle).

% 一个call填完所有的横，一个call填完所有的竖，然后看是否能把所有的词都填完
solve_puzzle_iteration(Puzzle0, WordList, NewWordList, Range, Puzzle1) :-
    length(WordList, MaxRange),
    Range > MaxRange,
    Puzzle1 = Puzzle0,
    NewWordList = WordList.
solve_puzzle_iteration(Puzzle0, WordList, NewWordList, Range, Puzzle) :-
    length(WordList, MaxRange),
    Range =< MaxRange,
    parse_puzzle(Puzzle0, WordList, WordList1, Range, [], Puzzle1),
    Range1 is Range + 1,
    solve_puzzle_iteration(Puzzle1, WordList1, NewWordList, Range1, Puzzle).


parse_puzzle([], WordList2, WordList2, _, Puzzle, Puzzle).
parse_puzzle([Row|RestPuzzle], WordList, WordList2, Range, TempPuzzle, Puzzle) 
    :-
    parse_row(Row, [], [], AllSlots),
    parse_slots(AllSlots, WordList, WordList1, Row, SolvedRow, Range),
    append(TempPuzzle, [SolvedRow], TempPuzzle1),
    parse_puzzle(RestPuzzle, WordList1, WordList2, Range, TempPuzzle1, Puzzle).


parse_row([], CurrentSlot, TempAllSlots, AllSlots) :-
    append(TempAllSlots, [CurrentSlot], AllSlots).
parse_row(['#'|RestRow], CurrentSlot, TempAllSlots, AllSlots) :-
    append(TempAllSlots, [CurrentSlot], AllSlots1),
    parse_row(RestRow, [], AllSlots1, AllSlots).
parse_row([Char|RestRow], CurrentSlot, TempAllSlots, AllSlots) :-
    Char \= '#',
    append(CurrentSlot, [Char], CurrentSlot1),
    parse_row(RestRow, CurrentSlot1, TempAllSlots, AllSlots).


parse_slots([], NewWordList, NewWordList, SolvedRow, SolvedRow, _).
parse_slots([[]|RestSlots], WordList, NewWordList, TempRow, SolvedRow, Range) :-
    parse_slots(RestSlots, WordList, NewWordList, TempRow, SolvedRow, Range).
parse_slots([Slot|RestSlots], WordList, NewWordList, TempRow, SolvedRow, Range) 
    :-
    Slot \= [],
    find_all_match_words(Slot, WordList, Words),
    fill_slots(TempRow, Slot, Words, WordList, WordList1, PartialRow,
               UnsolvedRow, Range),
    parse_slots(RestSlots, WordList1, NewWordList, 
                UnsolvedRow, PartialRow2, Range),
    append(PartialRow, PartialRow2, SolvedRow).


fill_slots(TempRow, _, Words, WordList, WordList1, PartialRow, 
           UnsolvedRow, Range) :-
    length(Words, NumberCandidates),
    NumberCandidates > 0,
    NumberCandidates =< Range,
    % string_chars(Word, SingleWordList),
    member(Word, Words),
    fill_row_with_word(TempRow, Word, PartialRow, UnsolvedRow, []),
    delete(WordList, Word, WordList1).
%TODO: if we are not filling this slot, how can we generate next unsolved row
fill_slots(TempRow, Slot, Words, WordList, WordList1, PartialRow,
           UnsolvedRow, Range) :-
    length(Words, NumberCandidates),
    NumberCandidates > Range,
    % Fill the empty slot into the original row, get the unsolved row
    fill_row_with_word(TempRow, Slot, PartialRow, UnsolvedRow, []),
    WordList1 = WordList.
fill_slots(TempRow, Slot, Words, WordList, WordList1, PartialRow,
           UnsolvedRow, _) :-
    length(Words, NumberCandidates),
    NumberCandidates = 0,
    % Fill the empty slot into the original row, get the unsolved row
    fill_row_with_word(TempRow, Slot, PartialRow, UnsolvedRow, []),
    delete(WordList, Slot, WordList1).


fill_row_with_word(['#'|RestRow], Word, PartialRow, UnsolvedRow, Solid) :-
    append(Solid, ['#'], Solid1),
    fill_row_with_word(RestRow, Word, PartialRow, UnsolvedRow, Solid1).
fill_row_with_word([Char|RestRow], Word, PartialRow, UnsolvedRow, Solid) :-
    Char \= '#',
    append(Solid, Word, PartialRow),
    length(Word, LengthSlot),
    length(Slot, LengthSlot),
    append(Slot, UnsolvedRow, [Char|RestRow]).


% find all match words from the word list and add them into a list Words
find_all_match_words(Slot, WordList, Words) :-
    findall(Word, find_match_word(Slot, WordList, Word), Words).


% find a match word from the word list
find_match_word(Slot, WordList, Word) :-
    member(Word, WordList),
    length(Slot, Length),
    length(Word, Length),
    match_word(Slot, Word).
    

% Check whether one slot and one word match each other
match_word([], []).
match_word(["_"|RestSlot], [_|RestWord]) :-
    match_word(RestSlot, RestWord).
match_word(['_'|RestSlot], [_|RestWord]) :-
    match_word(RestSlot, RestWord).
match_word([Char|RestSlot], [Char|RestWord]) :-
    match_word(RestSlot, RestWord).