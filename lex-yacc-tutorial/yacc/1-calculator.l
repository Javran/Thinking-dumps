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
[-+()=/*\n]             { return *yytext; }
    /* skip whitespace */
[ \t]                   ;
    /* anything else is an error */

.                       yyerror("invalid character");

%%

int yywrap(void) {
    return 1;
}
