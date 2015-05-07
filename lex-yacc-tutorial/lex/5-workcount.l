%{
    int nchar, nword, nline;
%}

%%

\n              { ++nline; ++nchar; }
[^ \t\n]        { ++nword; nchar += yyleng; }
.               { ++nchar; }

%%

int yywrap(void) {
    return 1;
}

int main(void) {
    yylex();
    printf("%d\t%d\t%d\n", nchar, nword, nline);
    return 0;
}
