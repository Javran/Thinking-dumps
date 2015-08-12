#ifndef JAVEV_POINTERMANAGER_H
#define JAVEV_POINTERMANAGER_H

// the inner struct is intentionally not exposed for isolation

// note that the handle is supposed to be an identifier
// of the allocated resource, we might change the content of the resource
// but the "handler" (e.g. pointer) is not supposed to be changed.
// thus we add "const" to it.
// "realloc" might break this assumption.
typedef const void *PHandle;
typedef void (*PFreeCallback)(PHandle);

void pointerManagerInit();
void pointerManagerFinalize();
void pointerManagerRegister(PHandle);
void pointerManagerRegisterCustom(PHandle,PFreeCallback);

#endif
