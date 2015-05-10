%token INTEGER VARIABLE
%left '+' '-'
%left '*' '/'

%{
    #include <stdio.h>
    void yyerror(char *);
    int yylex(void);
    // the symbol table is where
    // the variables get looked up
    int sym[26];
%}

%%

program:
        // a program is either of:
        // * a program with an extra statement and newline
        // * empty
        program statement '\n'
        |
        ;

statement:
        expr                    { printf("%d\n", $1); }
        | VARIABLE '=' expr     { sym[$1] = $3; }
        ;


expr:
        INTEGER
        // note that the variable contains only one character
        | VARIABLE          { $$ = sym[$1]; }
        | expr '+' expr     { $$ = $1 + $3; }
        | expr '-' expr     { $$ = $1 - $3; }
        | expr '*' expr     { $$ = $1 * $3; }
        | expr '/' expr     { $$ = $1 / $3; }
        | '(' expr ')'      { $$ = $2; }
        ;

%%

void yyerror(char *s) {
    fprintf(stderr, "%s\n", s);
}

int main(void) {
    yyparse();
    return 0;
}
