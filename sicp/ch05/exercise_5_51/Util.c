#include "Util.h"

char isFileExist(const char *pathName) {
    // Note that return value is zero on success,
    // We'd better say it explicitly
    return 0 == access(pathName, F_OK);
}

char isFileReadable(const char *pathName) {
    return 0 == access(pathName, R_OK);
}

// allocate and copy string content
// properly freeing is required to be done.
char *allocCopyString(const char *src) {
    size_t len = strlen(src);
    char *dst = calloc(1,len+1);
    return strncpy(dst, src, len);
}
