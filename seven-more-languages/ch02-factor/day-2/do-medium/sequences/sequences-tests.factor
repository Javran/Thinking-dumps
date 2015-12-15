IN: day-2.do-medium.sequences.tests

USE: day-2.do-medium.sequences
USING: tools.test math kernel ;

! current answer kept
{ { t "answer" } } [ { t "answer" } "another" [ t ] reduce-helper ] unit-test

! current answer not updated
{ { f f } } [ { f f } 7 [ 8 = ] reduce-helper ] unit-test

! current answer updated
{ { t 7 } } [ { f f } 7 [ 6 > ] reduce-helper ] unit-test
