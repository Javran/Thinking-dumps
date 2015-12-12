USING: tools.test io io.streams.null kernel namespaces sequences ;

USE: day-2.do-easy.strings

IN: day-2.test-suite

: test-all-examples ( -- )
    [ "day-2.do-easy" test ] with-null-writer
    test-failures get empty?
    [ "All tests passed." print ] [ :test-failures ] if ;

MAIN: test-all-examples
