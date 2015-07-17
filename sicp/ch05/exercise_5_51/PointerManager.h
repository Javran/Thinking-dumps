#ifndef JAVEV_POINTERMANAGER_H
#define JAVEV_POINTERMANAGER_H

// the inner struct is intentionally not exposed for isolation

typedef void *PHandle;
typedef void (*PFreeCallback)(PHandle);

void pointerManagerInit();
void pointerManagerFinalize();
void pointerManagerRegister(PHandle);
void pointerManagerRegisterCustom(PHandle,PFreeCallback);

#endif
