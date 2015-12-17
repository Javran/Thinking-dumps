USING: tools.test io io.streams.null kernel namespaces sequences ;

USE: day-2.do-easy.strings
USE: day-2.do-medium.sequences

IN: day-2.test-suite

USING: prettyprint ;

: quiet-test-with-info ( prefix -- )
    dup dup "++ Testing: " write print
    [ test ] with-null-writer
    "-- Test done: " write print
    ;

: test-all-examples ( -- )
    "day-2.do-easy" quiet-test-with-info
    "day-2.do-medium" quiet-test-with-info
    test-failures get empty?
    [ "All tests passed." print ]
    [ "Number of Tests failed: " print
      test-failures get length .
    ] if ;

MAIN: test-all-examples
