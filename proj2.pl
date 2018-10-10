% Declarative Programming Project 2
% Author : Zheping Liu, 683781, zhepingl
% This file includes the main program and some utility functions 
% for Declarative Programming project.

% The main program will read a puzzle file, a wordlist file, solve the puzzle
% and wirte the solution to solution file. Other utility functions includes
% functionality for reading files (read_file, read_lines, read_line);
% printing puzzles to files (print_puzzle, print_row, print_puzzle_char);
% validating puzzles (valid_puzzle) and its helper function same_length.
% Where the function solve_puzzle will be included in another file 
% solve_puzzle.pl, which includes all necessary functions to find the solution
% to a particular puzzle with a given word list.

:- ensure_loaded(solve_puzzle).

% main program that reads the puzzle file, word list file, solve the puzzle and
% print out the solution to solution file given.
main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

% read a file
read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

% read lines in a file
read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).

% read a line and check if it is the last line in the file
read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).

% print a puzzle to a solution file
print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

% print one row
print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

% print one character
put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

% verify if the puzzle is a valid puzzle
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(samelength(Row), Rows).

% check if two lists are with the same length
samelength([], []).
samelength([_|L1], [_|L2]) :-
	same_length(L1, L2).