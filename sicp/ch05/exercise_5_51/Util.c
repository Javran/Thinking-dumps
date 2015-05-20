#include "Util.h"

char isFileExist(const char *pathName) {
    // Note that return value is zero on success,
    // We'd better say it explicitly
    return 0 == access(pathName, F_OK);
}

char isFileReadable(const char *pathName) {
    return 0 == access(pathName, R_OK);
}
