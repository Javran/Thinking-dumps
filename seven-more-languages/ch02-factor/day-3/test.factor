IN: test

USING: kernel prettyprint math ;

TUPLE: foo bar baz ;

T{ foo f 10 20 } 

[ 200 + ] change-bar .
