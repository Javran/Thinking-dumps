#ifndef JAVEV_UTIL_H
#define JAVEV_UTIL_H

#include <unistd.h>
#include "Common.h"

char isFileExist(const char *);
char isFileReadable(const char *);
char *allocCopyString(const char *);

// free const pointers
static inline void freeC(const void *p) {
    // for historical reason, "free" on a const pointer
    // causes warnings. we suppress that by doing this conversion
    free((void *)p);
}

#endif
