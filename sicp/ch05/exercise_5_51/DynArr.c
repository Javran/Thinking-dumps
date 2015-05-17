// a simple self-adjusting array

// TODO: about free:
// so far I'm coding but haven't make
// any principle about coding in C.
// one rule I have to set up here is about
// the responsibility of freeing allocated objects,
// let's just say:
// * a procedure that frees an object, is only responsible
//   for freeing the objects' reachable inner structures.
//   and the responsiblity of freeing the object itself
//   should be taken care by the caller to this free procedure.
