## a queue implementation that blocks when the queue is empty and waits for new elements 

At first look I don't know what's the use of these kinds of queue,
why not just simply return some equivalent of 'null' rather than keep the thread sleeping/blocking.

After doing some search online, I came across 
[this article](http://www.javamex.com/tutorials/blockingqueue_example.shtml)
that demonstrates [BlockingQueue](http://docs.oracle.com/javase/6/docs/api/java/util/concurrent/BlockingQueue.html) 
and what the use of it in Java.

BlockingQueue is for background / lazy worker threads, 
which keeping them inactive when no task is assigned to them 
so that the overhead will be lower.

What we're looking for is a counterpart of BlockingQueue -- that is [fill-queue](http://richhickey.github.com/clojure-contrib/seq-utils-api.html),
the code of which is located [here](https://github.com/richhickey/clojure-contrib/blob/master/src/main/clojure/clojure/contrib/seq_utils.clj#L189)
