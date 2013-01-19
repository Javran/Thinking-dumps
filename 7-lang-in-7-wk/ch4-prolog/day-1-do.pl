% init
:- initialization(['common.pl']).
:- initialization(run).

% facts

% Task #1: construct a simple database to descipt some books and their authors
writesBook(agathaChristie, theABCMurder).
writesBook(agathaChristie, afterTheFuneral).
writesBook(agathaChristie, andThenThereWereNone).

writesBook(arthurConanDoyle, aStudyInScarlet).
writesBook(arthurConanDoyle, theSignOfTheFour).

% Task #3: construct a knowledge database that descripts
%     musicians and instruments, together with these musicians' genres

% I've attempted to place these predicates in a more reasonable order
%     but it seems discontiguous predicate is not allowed 
%     sorry for the poor readability

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

usesInstrument(steveVai, guitar).
usesInstrument(steveVai, keyboards).
usesInstrument(tommyEmmanuel, guitar).
usesInstrument(kennyG, saxophone).
usesInstrument(eugeGroove, saxophone).

inGenres(steveVai, rock).
inGenres(tommyEmmanuel, folk).
inGenres(tommyEmmanuel, blues).
inGenres(tommyEmmanuel, pop).
inGenres(tommyEmmanuel, jazz).
inGenres(kennyG, jazz).
inGenres(eugeGroove, jazz).

genresUseInstrument(Genres, Instrument) :- 
	usesInstrument(Musician, Instrument),
	inGenres(Musician, Genres).

% queries

% Task #2: find all books written by same author
query(findall(Book, writesBook(agathaChristie, Book), _)).
query(findall(Book, writesBook(arthurConanDoyle, Book), _)).

% Task #3: find all musicians that play guitar
query(findall(Musician, usesInstrument(Musician, guitar), _)).

% Additional Task: find all instruments played by musicians
%    whos genres is jazz
query(findall(Instrument, genresUseInstrument(jazz, Instrument), _)). 
