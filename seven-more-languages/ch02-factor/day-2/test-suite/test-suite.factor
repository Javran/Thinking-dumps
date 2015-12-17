USING: tools.test io io.streams.null kernel namespaces sequences ;

USE: day-2.do-easy.strings
USE: day-2.do-medium.sequences

IN: day-2.test-suite

USING: prettyprint vocabs math ;

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

: test-all-examples ( -- )
    0 vocab-count set
    "day-2.do-easy" quiet-test-with-info
    "day-2.do-medium" quiet-test-with-info 
    test-failures get empty?
    [ "All tests passed." print ]
    [ "Number of Tests failed: " print
      test-failures get length .
    ] if
    "Number of vocabularies visited: " write
    vocab-count get .
    ;

MAIN: test-all-examples
