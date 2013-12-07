It depends on the implementation of `make-serializer`:

* If `make-serializer` is implemented using something like a mutex,
It's likely that Ben's suggestion will work and won't cause any difference.

* If `make-serializer` generates unique identities for every procedure passed in
Then that might be unsafe to reuse the "protected" procedure. Because by using an identity,
`make-serializer` might have procedures wait on some condition, and the condition might be an identity.
If there are more than one procedure using the same identity (by calling the cached protected procedure),
there might be a resource race between them.
