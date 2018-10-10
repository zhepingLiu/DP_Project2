% Declarative Programming Project 2
% Author : Zheping Liu, 683781, zhepingl
% This file contains all necessary functions to solve a given puzzle with a
% given word list.

% This file will take the puzzle and word list read by the main program in
% the file proj2.pl (solve_puzzle, solve_puzzle_iteration), solve it in both
% horizontal direction and vertical direction using the same word list, where
% solve one direction of the puzzle (parse_puzzle) is done by applying 
% following steps:
% Firstly, find all slots in the puzzle that needs to be filled by a word in 
% the given word list (parse_row); 
% Secondly, iterate through all slots, find matched words for each slot, if the
% number of candidates is in the range restriction, fill one of the candidates
% into the slot, remove this candidate from word list and continue till all
% words in the word list have been filled somewhere.

% Assumptions: 
% - the puzzle read by the main program in proj2.pl is correct, this file
% does not check the correctness of the puzzle passed in;
% - the passed in word list is the correct one for the given puzzle;
% - the solid cell in the puzzle is represented using '#', and the empty cell
%   is represented using '_';


:- ensure_loaded(proj2).
:- ensure_loaded(library(clpfd)).

% solve_puzzle(Puzzle0, WordList, Puzzle)
% should hold when Puzzle is a solved version of Puzzle0, with the
% empty slots filled in with words from WordList.  Puzzle0 and Puzzle
% should be lists of lists of characters (single-character atoms), one
% list per puzzle row.  WordList is also a list of lists of
% characters, one list per word.
solve_puzzle(Puzzle0, WordList, Puzzle) :-
    solve_puzzle_iteration(Puzzle0, WordList, 1, Puzzle).


% solve_puzzle_iteration will go through both the horizontal puzzle and the 
% verticle puzzle once, fill every slot with candidates less than the Range
% argument. Then it will generate the new Range argument and pass it to next
% iteration until the WordList is empty (no more words to fill).

% base case of solve_puzzle_iteration.
% Input:  Puzzle, the Puzzle needs to be solved;
%         [], the empty word list;
%         _, the Range specified, which does not matter in the final iteration;
% Output: Puzzle, the final solved puzzle.
solve_puzzle_iteration(Puzzle, [], _, Puzzle).

% Input:  Puzzle0, the Puzzle needs to be solved;
%         WordList, the word list to be used in this iteration;
%         Range, the range of number of candidates allowed;
% Output: Puzzle, the final solved puzzle.
solve_puzzle_iteration(Puzzle0, WordList, Range, Puzzle) :-
    % this predicate is only true when the Range is not greater than the length
    % of the WordList
    length(WordList, MaxRange),
    Range =< MaxRange,
    % parse the horizontal puzzle with Range, return the new word list 
    % WordList1, and the partially solved puzzle Puzzle1
    parse_puzzle(Puzzle0, WordList, WordList1, Range, [], Puzzle1),
    transpose(Puzzle1, TransPuzzle1),
    % check if there is any slot being filled by parsing the horizontal puzzle;
    % if so, the new range for parsing the vertical puzzle is 1;
    % otherwise, do not change the range (Range1 = Range)
    check_range2(Puzzle0, Puzzle1, Range, Range1),
    % parse the vertical puzzle with Range1, return the new word list
    % WordList2, and another partially solved puzzle Puzzle2
    parse_puzzle(TransPuzzle1, WordList1, WordList2, Range1, [], Puzzle2),
    % transpose Puzzle2 back to horizontal form (Puzzle3)
    transpose(Puzzle2, Puzzle3),
    % check if there is any slot being filled by parsing both horizontal and
    % vertical puzzle; 
    % if so, the range (Range2) for next iteration is 1;
    % otherwise, increment the range to lower the restriction.
    check_range(Puzzle0, Puzzle3, Range, Range2),
    % next iteration with partially solved puzzle, new word list, new range,
    % the output (Puzzle) is the output of the whole function.
    solve_puzzle_iteration(Puzzle3, WordList2, Range2, Puzzle).


% change the range for next parse depending on whether Puzzle0 equals Puzzle1
% Input:  Puzzle0, the first puzzle for comparing;
%         Puzzle1, the second puzzle for comparing;
%         Range, the original range;
% Output: Range1, the new range for next parsing.
check_range(Puzzle0, Puzzle1, Range, Range1) :-
    (
        Puzzle0 = Puzzle1
        % when two puzzles are same, increment the range to lower the 
        % restriction
    ->  Range1 is Range + 1
        % else, starts from minimum range again
    ;   Range1 = 1
    ).

% change the range for next parse depending on whether Puzzle0 equals Puzzle1
% Input:  Puzzle0, the first puzzle for comparing;
%         Puzzle1, the second puzzle for comparing;
%         Range, the original range;
% Output: Range1, the new range for next parsing.
check_range2(Puzzle0, Puzzle1, Range, Range1) :-
    (
        Puzzle0 = Puzzle1
        % when two puzzles are same, do not change range
    ->  Range1 = Range
        % else, starts from minimum range again
    ;   Range1 = 1
    ).


% parse_puzzle will iterate through all rows of a given puzzle, generate all
% slots in each row, fill in all possible slots using match words in the 
% argument WordList, with number of candidates smaller or equal than the 
% Range restriction. Finally, it will construct a partially solved puzzle by
% appending all partilly solved row in this iteration.

% base case of parse_puzzle, when there is no more row left in the puzzle,
% return the word list word list passed in as new word list for next parse,
% and return the temporary puzzle constructed as partially solved puzzle.
% Input:  []: empty puzzle, indicates no more rows to parse;
%         WordList2, the given word list;
%         _, argument Range, which is not used in the base case;
%         Puzzle, the temporary puzzle used to store the partially solved 
%                 puzzle;
% Output: WordList2, the returned new word list with all words used deleted;
%         Puzzle, the partially solved puzzle to return.
parse_puzzle([], WordList2, WordList2, _, Puzzle, Puzzle).

% Input:  [Row|RestPuzzle], the current row and rest of rows in the puzzle;
%         WordList, the word list to be used to fill slots in this round;
%         Range, the number of candidates allowed;
%         TempPuzzle, the temporary puzzle used to store partially sovled rows;
% Output: WordList2, the word list with used words in this round deleted;
%         Puzzle, the complete partially solved puzzle (with all rows).
parse_puzzle([Row|RestPuzzle], WordList, WordList2, Range, TempPuzzle, Puzzle) 
    :-
    % parse the row, generate all slots in this row (AllSlots)
    parse_row(Row, [], [], AllSlots),
    % parse all slots in this row, fill them using WordList, return the 
    % partially solved row (SolvedRow) and new word list (WordList1)
    parse_slots(AllSlots, WordList, WordList1, Row, SolvedRow, Range),
    % append this row to the temporary puzzle, make it TempPuzzle1
    append(TempPuzzle, [SolvedRow], TempPuzzle1),
    % parse the rest of rows of puzzle with new word list and new temporary
    % puzzle
    parse_puzzle(RestPuzzle, WordList1, WordList2, Range, TempPuzzle1, Puzzle).


% parse_row will parse one row of the puzzle, iterate through all chars in the
% row, and return all slots in that row.

% base case of parse_row, when there is no more char left in the row, append
% current slot under construction to the temporary all slots to build AllSlots.
% Input:  []: the empty row, indicates there is no more char in this row;
%         CurrentSlot, current slot under construction;
%         TempAllSlots, temporary all slots store all slots constructed before;
% Output: AllSlots, a list of all slots in this row.
parse_row([], CurrentSlot, TempAllSlots, AllSlots) :-
    append(TempAllSlots, [CurrentSlot], AllSlots).

% case when the currrent reading Char is a '#' (solid), append the current slot
% under construction to TempAllSlots, and pass the rest row, a new (empty) slot
% to construct and new TempAllSlots1 to next iteration.
% Input:  ['#'|RestRow]: the current char (solid) and rest row;
%         CurrentSlot, current slot under construction;
%         TempAllSlots, temporary all slots store all slots constructed before;
% Output: AllSlots, a list of all slots in this row.
parse_row(['#'|RestRow], CurrentSlot, TempAllSlots, AllSlots) :-
    append(TempAllSlots, [CurrentSlot], TempAllSlots1),
    parse_row(RestRow, [], TempAllSlots1, AllSlots).

% case when the current reading Char is not solid, append the current char
% to the current slot under construction, and pass the rest row, current 
% incomplete slot to construct, and current TempAllSlots to next iteration.
% Input:  [Char|RestRow]: the current char and rest row;
%         CurrentSlot, current slot under construction;
%         TempAllSlots, temporary all slots store all slots constructed before;
% Output: AllSlots, a list of all slots in this row.
parse_row([Char|RestRow], CurrentSlot, TempAllSlots, AllSlots) :-
    Char \= '#',
    append(CurrentSlot, [Char], CurrentSlot1),
    parse_row(RestRow, CurrentSlot1, TempAllSlots, AllSlots).


% parse_slots will iterate through all slots in a row, fill in all
% possible slots using match words in the argument WordList, with number of 
% candidates smaller or equal than the Range restriction. Finally it will
% construct a complete partially solved row.

% base case of parse_slots, when there is no more slots left, return the new
% word list and the complete partially solved row.
% Input:  [], empty list of slots, indicates there is no more slot to parse;
%         NewWordList, the new word list with all used words deleted;
%         SolvedRow, the temporary partially solved row constructed before;
%         _, the argument Range, which is not used in base case;
% Output: NewWordList, the new word list with all used words deleted;
%         SolvedRow, the temporary partially solved row constructed before;
parse_slots([], NewWordList, NewWordList, SolvedRow, SolvedRow, _).

% case when the current slot is empty (not a real slot), skip it to parse
% next one.
parse_slots([[]|RestSlots], WordList, NewWordList, Row, SolvedRow, Range) :-
    parse_slots(RestSlots, WordList, NewWordList, Row, SolvedRow, Range).

% case when the slot is a real slot, fill this slot using matched words in 
% word list, then parse next slot. At the end, construct a complete partially
% filled row by appending all partial rows.
% Input:  [Slot|RestSlots], the current slot and rest slots to be parsed;
%         WordList, the word list stores all unused words;
%         TempRow, the current (partial) row to parse;
%         Range, the restriction on the number of candidates;
% Output: NewWordList, the new word list with all used words deleted;
%         SolvedRow, the temporary partially solved row constructed before;
parse_slots([Slot|RestSlots], WordList, NewWordList, TempRow, SolvedRow, Range) 
    :-
    % ensure slot is not []
    Slot \= [],
    % return a list of all matched words (Words) with this slot
    find_all_match_words(Slot, WordList, Words),
    % fill the slot using words in the list of matched words, return the
    % new word list with used word deleted, the partial row with filled 
    % slot, and the rest of the row to fill (UnsolvedRow)
    fill_slots(TempRow, Slot, Words, WordList, WordList1, PartialRow, 
               UnsolvedRow, Range),
    % parse the next slot using new word list and the unsolved row
    parse_slots(RestSlots, WordList1, NewWordList, 
                UnsolvedRow, PartialRow2, Range),
    % append the partial rows to make one complete partially solved row
    append(PartialRow, PartialRow2, SolvedRow).


% fill_slots will fill the row with a word in the list of matched words, return
% the new word list with used words deleted, the partial row solved and the
% unsolved row.

% case when the number of candidates is in the range required, fill the slot
% with one word in the list of candidates.
% Input:  TempRow, the temp row to solve;
%         _, current slot, it is not used in this case;
%         Words, a list of matched words for this slot;
%         WordList, the word list with all unused words;
%         Range, the restriction on number of candidates;
% Output: WordList1, the new word list with all used words deleted;
%         PartialRow, the partial row has been parsed;
%         UnsolvedRow, the rest of TempRow that is un-parsed;
fill_slots(TempRow, _, Words, WordList, WordList1, PartialRow, 
           UnsolvedRow, Range) :-
    % when the number of cnadidates is below range restriction and is greater
    % than 0
    length(Words, NumberCandidates),
    NumberCandidates > 0,
    NumberCandidates =< Range,
    % find a word in the list of matched words
    member(Word, Words),
    % fill the slot with that word
    fill_row_with_word(TempRow, Word, PartialRow, UnsolvedRow, []),
    % make a new word list by deleting the used word from old word list
    delete(WordList, Word, WordList1).

% case when the number of candidates is greater than the range restriction,
% fill the slot back into the slot (no modification to it), the new word list
% is exact the same as old one.
% Input:  TempRow, the temp row to solve,
%         Slot, the current slot being parsed,
%         Words, the list of words match the current slot,
%         WordList, the list of words that are unused,
%         Range, the restriction on the number of candidates,
% Output: WordList1, the new word list with used word deleted,
%         PartialRow, the partial row that has been parsed,
%         UnsolvedRow, the rest of TempRow that hasn't been parsed.
fill_slots(TempRow, Slot, Words, WordList, WordList1, PartialRow,
           UnsolvedRow, Range) :-
    length(Words, NumberCandidates),
    NumberCandidates > Range,
    % Fill the empty slot into the original row, get the unsolved row
    fill_row_with_word(TempRow, Slot, PartialRow, UnsolvedRow, []),
    WordList1 = WordList.

% case when the number of candidates is 0, usually indicates the slot is being
% filled before, fill the original slot into the slot (no modification to it),
% the new word list is exact the same as old one.
% Input:  TempRow, the temp row to solve,
%         Slot, the current slot being parsed,
%         Words, the list of words match the current slot,
%         WordList, the list of words that are unused,
%         _, the restriction on the number of candidates, not used in this case,
% Output: WordList1, the new word list with used word deleted,
%         PartialRow, the partial row that has been parsed,
%         UnsolvedRow, the rest of TempRow that hasn't been parsed.
fill_slots(TempRow, Slot, Words, WordList, WordList1, PartialRow,
           UnsolvedRow, _) :-
    length(Words, NumberCandidates),
    NumberCandidates = 0,
    % Fill the original slot into the original row, get the unsolved row
    fill_row_with_word(TempRow, Slot, PartialRow, UnsolvedRow, []),
    WordList1 = WordList.


% fill the row (slot) with a specified word.

% case when the Char of the row is '#' (solid), append the solid to the current
% Solid list, and skip to next Char.
% Input:  ['#'|RestRow], the current Char and rest of the row,
%         Word, the sepcified word to fill,
%         Solid, the current solid part under construction,
% Output: PartialRow, the partial row has been parsed,
%         UnsolvedRow, the rest row has not been parsed
fill_row_with_word(['#'|RestRow], Word, PartialRow, UnsolvedRow, Solid) :-
    append(Solid, ['#'], Solid1),
    fill_row_with_word(RestRow, Word, PartialRow, UnsolvedRow, Solid1).

% case when the Char of the row is not solid, create partial row by appending 
% the solid part to the Word, create unsolved row by removing the same 
% length of chars as Word from the TempRow ([Char|RestRow])
% Input:  [Char|RestRow], the current Char and rest of the row,
%         Word, the sepcified word to fill,
%         Solid, the current solid part under construction,
% Output: PartialRow, the partial row has been parsed,
%         UnsolvedRow, the rest row has not been parsed
fill_row_with_word([Char|RestRow], Word, PartialRow, UnsolvedRow, Solid) :-
    Char \= '#',
    append(Solid, Word, PartialRow),
    length(Word, LengthSlot),
    length(Pre, LengthSlot),
    % find UnsolvedRow by removing the slot (same length as Word) from TempRow
    append(Pre, UnsolvedRow, [Char|RestRow]).


% find all match words for a specified slot from the given word list
% and add them into a list Words.
% Input:  Slot, the specified slot,
%         WordList, the given word list,
% Output: the list of match words.
find_all_match_words(Slot, WordList, Words) :-
    findall(Word, find_match_word(Slot, WordList, Word), Words).


% find a match word for the specified slot from the given word list.
% and add them into a list Words.
% Input:  Slot, the specified slot,
%         WordList, the given word list,
% Output: a single match word.
find_match_word(Slot, WordList, Word) :-
    % the length of Slot and the Word has to be the same
    length(Slot, Length),
    member(Word, WordList),
    length(Word, Length),
    % check whether a word with the same length of Slot matches it
    match_word(Slot, Word).


% Check whether one slot and one word match each other
% Input: Slot, the given slot,
%        Word, the given word.

% base case of match_word, true when both are empty lists.
match_word([], []).

% case when the Slot has a '_', match any Char from the Word to it.
match_word(['_'|RestSlot], [_|RestWord]) :-
    % comparing the rest of slot and word
    match_word(RestSlot, RestWord).

% case when the Slot has specific Char, match only the same Char from the Word
% to it.
match_word([Char|RestSlot], [Char|RestWord]) :-
    % comparing the rest of slot and word
    match_word(RestSlot, RestWord).