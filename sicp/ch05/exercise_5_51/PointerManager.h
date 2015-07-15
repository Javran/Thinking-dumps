#ifndef _JAVEV_POINTERMANAGER_H_
#define _JAVEV_POINTERMANAGER_H_

// the inner struct is intentionally not exposed for isolation

typedef void *PHandle;
typedef void (*PFreeCallback)(PHandle);

void pointerManagerInit();
void pointerManagerFinalize();
void pointerManagerRegister(PHandle);
void pointerManagerRegisterCustom(PHandle,PFreeCallback);

#endif
