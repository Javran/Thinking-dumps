## For the sizer program, What will happen if you did not create a new actor for each link?

The most obvious difference between `getPageSizeSequenctially` and `getPageSizeConcurrently` is that
the latter uses actors to fetch data for each link. 

So if we don't create a new actor for each link, the fetching process will be performed sequentially rather than concurrently.
The performance of `getPageSizeConcurrently`(so-called) will be roughly the same as that of `getPageSizeSequenctially`.
