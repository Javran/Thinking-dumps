%{
        // Don't use yylineno here, which is used by lex already
        int lineno = 0;
%}
%%
^(.*)\n  printf("%4d\t%s", ++lineno, yytext);
%%

int yywrap(void) {
    return 1;
}

int main(void) {
    yylex();
    return 0;
}
