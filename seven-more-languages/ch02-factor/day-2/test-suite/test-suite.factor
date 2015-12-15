USING: tools.test io io.streams.null kernel namespaces sequences ;

USE: day-2.do-easy.strings
USE: day-2.do-medium.sequences

IN: day-2.test-suite

: test-all-examples ( -- )
    ! I wish to see that's going on,
    ! and the number of testcases is not too much
    ! so let's don't mute the output
    "day-2.do-easy" test
    "day-2.do-medium" test
    test-failures get empty?
    [ "All tests passed." print ] [ :test-failures ] if ;

MAIN: test-all-examples
