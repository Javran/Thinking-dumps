%%
        /* match everything except newline */
.       ECHO;
        /* match newline */
\n      ECHO;

%%

int yywrap(void) {
    return 1;
}

int main(void) {
    // there is previously a "1-minimal.l"
    // which does not work without extra "main.c" files.
    yylex();
    return 0;
}
