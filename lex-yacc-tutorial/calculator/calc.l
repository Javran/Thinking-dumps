%{
    #include <stdlib.h>
    #include "y.tab.h"

    void yyerror(char *);
%}

%%

[a-z]           {
                    // a variable TODO: sIndex??
                    yylval.sIndex = *yytext - 'a';
                    return VARIABLE;
                }

0               {
                    yylval.iValue = atoi(yytext);
                    return INTEGER;
                }


[1-9][0-9]*     {
                    // I suspect this is to prevent things like "0000"
                    yylval.iValue = atoi(yytext);
                    return INTEGER;
                }

[-()<>=+*/;{}.] {   // get that symbol and return
                    return *yytext;
                }

">="            return GE;
"<="            return LE;
"=="            return EQ;
"!="            return NE;
"while"         return WHILE;
"if"            return IF;
"else"          return ELSE;
"print"         return PRINT;
[ \t\n]+        ;       /* ignore whitespace */

.               yyerror("Unknown character");


%%

int yywrap(void) {
    return 1;
}
