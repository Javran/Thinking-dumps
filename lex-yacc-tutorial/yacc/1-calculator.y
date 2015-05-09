%{
    #include <stdio.h>
    int yylex(void);
    void yyerror(char *);
%}

%token INTEGER

%%

program:
    program expr '\n'   { printf("%d\n", $2); }
    |
    ;

expr:
    INTEGER             {
                          // this is actually the default action
                          // not necessary to specify it
                          $$ = $1;
                        }
    | expr '+' expr     { $$ = $1 + $3; }
    | expr '-' expr     { $$ = $1 - $3; }
    ;

%%

void yyerror(char *s) {
    fprintf(stderr, "%s\n", s);
}

int main(void) {
    yyparse();
    return 0;
}
