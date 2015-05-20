#include "Common.h"
#include "Syntax.h"

int isLetter(char c) { return isalpha(c); }

int isSpecialInitial(char c) {
    static const char *sp = "!$%&*/:<=>?^_~";
    const int spLen = strlen(sp);
    int i;
    for (i=0; i<spLen; ++i)
        if (sp[i] == c)
            return 1;
    return 0;
}

int isPeculiarIdentifierInitial(char c) {
    // I don't want to worry about "..." here
    return '+' == c || '-' == c;
}

int isSpecialSubsequent(char c) {
    return '+' == c || '-' == c
        || '.' == c || '@' == c;
}

int isInitial(char c) {
 return isLetter(c)
     || isSpecialInitial(c)
     || isPeculiarIdentifierInitial(c);
}

int isSubsequent(char c) {
    return isInitial(c)
        || isdigit(c)
        || isSpecialSubsequent(c);
}
