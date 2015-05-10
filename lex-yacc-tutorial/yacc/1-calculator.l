%{
    #include <stdlib.h>
    void yyerror(char *);
    #include "y.tab.h"
%}

%%

    /* variables */
    // variable contains exactly one character
[a-z]                   {
                            // and the range is from 'a' to 'z'
                            // with 'a' indexed as 0
                            yylval = *yytext - 'a';
                            return VARIABLE;
                        }

    /* integers */
[0-9]+                  {
                            yylval = atoi(yytext);
                            return INTEGER;
                        }

    /* operators */
[-+()=/*\n]             {
                            // by doing so, we must
                            // make sure operator values
                            // do not conflict with
                            // VARIABLE / INTEGER / etc.
                            return *yytext;
                        }
    /* skip whitespace */
[ \t]                   ;   // skip them
    /* anything else is an error */

.                       yyerror("invalid character");

%%

// seems like there are two ways of returning
// parse results:
// * use a return value
// * assign something to yylval
// TODO: roles that yacc and lex play?

int yywrap(void) {
    return 1;
}
