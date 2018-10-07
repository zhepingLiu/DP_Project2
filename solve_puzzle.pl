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

solve_puzzle(Puzzle0, WordList, Puzzle, SolutionFile1, SolutionFile2) :-
    % transpose(Puzzle0, TransPuzzle),
    parse_puzzle(Puzzle0, WordList, WordList1, [], Puzzle),
    transpose(Puzzle, TransPuzzle),
    print_puzzle(SolutionFile1, Puzzle),
    % Empty word list is to make sure there is no word left at the end
    parse_puzzle(TransPuzzle, WordList1, [], [], Puzzle1),
    print_puzzle(SolutionFile2, Puzzle1),
    transpose(Puzzle1, Puzzle).


parse_puzzle([], WordList2, WordList2, Puzzle, Puzzle).
parse_puzzle([Row|RestPuzzle], WordList, WordList2, TempPuzzle, Puzzle) :-
    parse_row(Row, [], [], AllSlots),
    % parse_slots(AllSlots, 0, WordList, WordList1, Row, SolvedRow),
    parse_slots(AllSlots, WordList, WordList1, Row, SolvedRow),
    append(TempPuzzle, [SolvedRow], TempPuzzle1),
    parse_puzzle(RestPuzzle, WordList1, WordList2, TempPuzzle1, Puzzle).


parse_row([], CurrentSlot, TempAllSlots, AllSlots) :-
    append(TempAllSlots, [CurrentSlot], AllSlots).
parse_row(['#'|RestRow], CurrentSlot, TempAllSlots, AllSlots) :-
    append(TempAllSlots, [CurrentSlot], AllSlots1),
    parse_row(RestRow, [], AllSlots1, AllSlots).
parse_row([Char|RestRow], CurrentSlot, TempAllSlots, AllSlots) :-
    Char \= '#',
    append(CurrentSlot, [Char], CurrentSlot1),
    parse_row(RestRow, CurrentSlot1, TempAllSlots, AllSlots).


parse_slots([], NewWordList, NewWordList, SolvedRow, SolvedRow).
parse_slots([[]|RestSlots], WordList, NewWordList, TempRow, SolvedRow) :-
    parse_slots(RestSlots, WordList, NewWordList, TempRow, SolvedRow).
parse_slots([Slot|RestSlots], WordList, NewWordList, TempRow, SolvedRow) :-
    Slot \= [],
    find_match_word(Slot, WordList, Word),
    Word \= [],
    string_chars(Word, SingleWordList),
    fill_row_with_word(TempRow, SingleWordList, PartialRow, UnsolvedRow, []),
    delete(WordList, Word, WordList1),
    parse_slots(RestSlots, WordList1, NewWordList, UnsolvedRow, PartialRow2),
    append(PartialRow, PartialRow2, SolvedRow).
parse_slots([Slot|RestSlots], WordList, NewWordList, TempRow, SolvedRow) :-
    Slot \= [],
    find_match_word(Slot, WordList, Word),
    Word = [],
    parse_slots(RestSlots, WordList, NewWordList, TempRow, SolvedRow).


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
find_match_word(_, [], Word) :-
    Word = [].
find_match_word(Slot, WordList, Word) :-
    length(Slot, Length),
    member(Word, WordList),
    length(Word, Length),
    string_chars(Word, SingleWordList),
    match_word(Slot, SingleWordList).
find_match_word(Slot, WordList, Word) :-
    length(Slot, Length),
    member(Word, WordList),
    length(Word, Length1),
    Length \= Length1,
    Word = [].
    

% Check one slot and one word match each other
match_word([], []).
match_word(["_"|RestSlot], [_|RestWord]) :-
    match_word(RestSlot, RestWord).
match_word(['_'|RestSlot], [_|RestWord]) :-
    match_word(RestSlot, RestWord).
match_word([Char|RestSlot], [Char|RestWord]) :-
    match_word(RestSlot, RestWord).