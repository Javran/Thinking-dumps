% init
:- initialization(['common.pl']).
:- initialization(run).

% facts

% Task #1: construct a simple database to descipt some books and their authors
writes_book(agathaChristie, theABCMurder).
writes_book(agathaChristie, afterTheFuneral).
writes_book(agathaChristie, andThenThereWereNone).

writes_book(arthurConanDoyle, aStudyInScarlet).
writes_book(arthurConanDoyle, theSignOfTheFour).

% Task #3: construct a knowledge database that descripts
%     musicians and instruments, together with these musicians' genres

% it seems discontiguous predicate is not allowed by default
%     just let it be, coding in Prolog's way 
% but if you want to relax this rule, the links below might be useful:
% http://stackoverflow.com/questions/7400904/discontiguous-predicate-warning-from-gnu-prolog
% http://www.gprolog.org/manual/gprolog.html#htoc53

musician(steveVai).
musician(tommyEmmanuel).
musician(kennyG).
musician(eugeGroove).

instrument(guitar).
instrument(keyboards).
instrument(saxophone).

genres(rock).
genres(blues).
genres(jazz).
genres(folk).
genres(pop).

uses_instrument(steveVai     , guitar   ).
uses_instrument(steveVai     , keyboards).
uses_instrument(tommyEmmanuel, guitar   ).
uses_instrument(kennyG       , saxophone).
uses_instrument(eugeGroove   , saxophone).

in_genres(steveVai     , rock ).
in_genres(tommyEmmanuel, folk ).
in_genres(tommyEmmanuel, blues).
in_genres(tommyEmmanuel, pop  ).
in_genres(tommyEmmanuel, jazz ).
in_genres(kennyG       , jazz ).
in_genres(eugeGroove   , jazz ).

genres_use_instrument(Genres, Instrument) :- 
	uses_instrument(Musician, Instrument),
	in_genres(Musician, Genres).

% queries

% Task #2: find all books written by same author
query(findall(Book, writes_book(agathaChristie  , Book), _)).
query(findall(Book, writes_book(arthurConanDoyle, Book), _)).

% Task #3: find all musicians that play guitar
query(findall(Musician, uses_instrument(Musician, guitar), _)).

% Additional Task: find all instruments played by musicians
%    whos genres is jazz
query(findall(Instrument, genres_use_instrument(jazz, Instrument), _)). 
