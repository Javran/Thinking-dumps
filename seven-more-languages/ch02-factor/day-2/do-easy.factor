USING: kernel sequences prettyprint ;
IN: day-2.do-easy

USE: day-2.do-easy.strings
USE: tools.test

{ f } [ "abcde" palindrome? ] unit-test
{ t } [ "abcba" palindrome? ] unit-test
