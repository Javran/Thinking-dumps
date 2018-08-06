/*
   just few notes about doing binary search properly:

   (here we are talking about picking elements)

   - there are two ways to pick a middle element:

     - mid = (l+r) / 2

       I call this "left-biased" because when (l+r) is an even number,
       mid will be slightly closer to l

     - mid = (l+r+1) / 2

       I call this "right-biased" for a similar reason

     - note that when (l+r) is an odd number these 2 methods
       does not make a difference

   - now that to cut search space into half, we'll do two things

     - `l = mid` or `r = mid - 1`

       I call this "left-biased pruning",
       as it keeps `l` but shrinks `r` towards left side.

       in this case we'll be returning `l` as it'll be guaranteed to
       be remained in range (even if `i<j` is no longer satisfied)

     - `l = mid + 1` or `r = mid`

       I call this "right-biased pruning"
       as it keeps `r` but shrinks `l` towards right side.

       this case is similar, we'll be returning `r`

   - a rule of thumb:

     + left-biased mid-picking + right-biased pruning
     + right-biased mid-picking + left-biased pruning
     + never biased towards the same direction in mid-picking and pruning

     the intuition is simply to guarantee that every iteration
     we'll strictly shrink the size of the search space so to avoid infinite loops.

   - when in doubt, try l=0, r=1 and see if it goes into infinite loop.

 */
