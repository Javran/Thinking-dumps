USING: tools.test io io.streams.null kernel namespaces sequences ;

USE: day-2.do-easy.strings
USE: day-2.do-medium.sequences

IN: day-2.test-suite

USING: prettyprint vocabs math combinators ;

SYMBOL: vocab-count

! return number of child vocabularies
: quiet-test-with-info ( prefix -- )
    "++ Testing: " write dup print
    [
      dup test
    ] with-null-writer
    "-- Test done: " write dup print
    ! bump counter
    child-vocabs length
    vocab-count get +
    vocab-count set
    ;

! store a list of vocabulary prefixes
: test-vocab-prefixes ( -- seq )
    { "day-2.do-easy"
      "day-2.do-medium"
    }
    ;

! we are expected to count how many tests we have run,
! but looking at public interfaces provided by unit testing
! vocabulary we have, it might not be possible to count the number
! therefore, as the closest solution to the exercise, we print out
! number of vocabularies we have visited by "test",
! note that some vocabularies might not have unit tests,
! but there is no simple way to tell.
: test-all-examples ( -- )
    0 vocab-count set
    test-vocab-prefixes [ quiet-test-with-info ] each
    ! "day-2.do-easy" quiet-test-with-info
    ! "day-2.do-medium" quiet-test-with-info
    test-failures get empty?
    [ "All tests passed." print ]
    [ "Number of Tests failed: " print
      test-failures get length .
    ] if
    "Number of vocabularies visited: " write
    vocab-count get .
    ;

! use this entry for interactive test suite control
! TODO: impl
! plan: entry vocab prefix for individual vocabulary tests
! or use ":quit" to quit, ":test-all" for running all preset tests
: test-interactive ( -- )
    "Avaliable tests: " print
    test-vocab-prefixes [ "* " write print ] each
    "Input one from above for running that test suite, " print
    "or use \":quit\" or \":test-all\"" print flush
    readln
    { { [ dup ":quit" = ] [ drop "Quit." print ] }
      { [ dup ":test-all" = ] [ drop test-all-examples test-interactive ] }
      [ drop "TODO" print ] } cond
    ;

! MAIN: test-all-examples
MAIN: test-interactive

! TODO: summarize all problems we have in NOTES.md
