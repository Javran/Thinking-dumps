The matching process runs through the database entries one by one.
For each entry, it either produces an extension to the frame,
or indicates an error.

My note: this really reminds me the Writer monad and Maybe monad,
since on success the "information" will be accumulated through
frame-extension and that Maybe monad can be used to indicate
if the computation is successful or not (Here we are of course
talking about pattern matching)

My note 2: I don't know any detail about the implementation,
but I think the core idea is to generate all possible solutions
in spite of constraints (We use stream because it won't try to
give us all the answers at once). And after that, we apply limits
to the stream which will result in a stream of all valid solutions.
