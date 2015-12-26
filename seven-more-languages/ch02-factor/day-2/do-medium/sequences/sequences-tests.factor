IN: day-2.do-medium.sequences.tests

USE: day-2.do-medium.sequences
USING: tools.test math kernel sequences ;

! current answer kept
{ { t "answer" } }
[ { t "answer" } "another" [ t ] reduce-helper ] unit-test

! current answer not updated
{ { f f } } 
[ { f f } 7 [ 8 = ] reduce-helper ] unit-test

! current answer updated
{ { t 7 } } 
[ { f f } 7 [ 6 > ] reduce-helper ] unit-test

! test when combining with reduce
[ { t 8 } ]
[ { 1 2 3 4 5 5 1 3 8 } { f f } [ [ 5 > ] reduce-helper ] reduce ]
unit-test

! find-first: not found, raising an error
[ { 1 9 2 8 3 7 } [ 10 = ] find-first ] must-fail

! find-first: should find the first alternative
{ 5 } 
[ { 9 8 7 6 5 4 } [ 6 < ] find-first ] unit-test
