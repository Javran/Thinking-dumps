digit   [0-9]
letter  [A-Za-z]
%{
    int count;
%}
%%
    /* match identifier */
{letter}({letter}|{digit})*  ++count;

.      ; // does nothing on mismatches
\n     ;
%%

int yywrap(void) {
    return 1;
}

int main(void) {
    yylex();
    printf("number of identifiers = %d\n", count);
    return 0;
}
