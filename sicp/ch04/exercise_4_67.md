First of all, I have to emphasize that it is the programmer's fault to
put problematic rules into this system and I will make no attempt
to turn a problematic rule into a correct one, as it might be
a really complex task.

Therefore, when a loop is detected, we simply abort the current query
and signal an error. We could just return an empty stream instead,
but I think that makes a misleading situation where
the query system returns nothing and you think this is because
no valid results are found while the real story is that the query system
detects something wrong (in this case, an infinite loop)
but you have no feedback.

We solve this problem by studying the cases where the qeval system
get stuck into an infinte loop.

One example comes directly from exercise 4.64:

    (rule (outranked-by ?staff-person ?boss)
          (or (supervisor ?staff-person ?boss)
              (and (outranked-by ?middle-manager ?boss)
              (supervisor ?staff-person ?middle-manager))))

We get into trouble in this case because `(outranked-by ?staff-person ?boss)`
and the inner `(outranked-by ?middle-manager ?boss)` are essentially
the same query, without the constraints put by `supervisor` rule in the
inner `and` part, we actually don't make any progress between this two `outranked-by`
rules, therefore get stuck forever.

Based on this result, I give another example with might be a little bit
simplier to work with:

    (edge a b)
    (edge b c)
    (edge c d)

    (rule (link ?x ?y) (edge ?x ?y))
    (rule (link ?x ?z)
          (and (link ?y ?z)
               (edge ?x ?y)))

When we query about `(link a d)`, the query system will also result in
an infinite loop because we have flipped two sub-queries in `and` part
on purpose (The correct one should put `(edge ?x ?y)` earlier,
or otherwise `(link ?y ?z)` and `(link ?x ?y)` are essentially the same query
and we are actually making no progess.

To solve this problem, I began with observing the frames when
there is an infinite loop. And actually it turns out to be very useful.

For the first example, here is a pretty printed frame:

* `?boss-7 -> ?boss-8`
* `?middle-manager-7 -> ?staff-person-8`
* `?boss-6 -> ?boss-7`
* `?middle-manager-6 -> ?staff-person-7`
* `?boss-5 -> ?boss-6`
* `?middle-manager-5 -> ?staff-person-6`
* ...
* `?boss-1 -> ?boss-2`
* `?middle-manager-1 -> ?staff-person-2`
* `?who -> ?boss-1`
* `?staff-person-1 -> Bitdiddle Ben`

For the second example:

* `?y-6 -> ?z-7`
* `?x-7 -> a`
* `?y-5 -> ?z-6`
* `?x-6 -> a`
* ...
* `?y-1 -> ?z-2`
* `?x-2 -> a`
* `?z-1 -> a`
* `?x-1 -> a`

I believe the pattern is obvious: except for the early few bindings
(i.e. the last few bindings,
remember the frame grows by putting bindings in front of it),
the frame bindings are just replicating themselves but not
making anything new.

Now we put these bindings into groups, for the first example, `[?boss,?middle-manager]`
forms a group, and this group of bindings replicates for several times.
Similarly, for the second example, the group is `[?y,?x]`.
To detect a loop, we can just examine the frame,
if we can find the first two binding groups, we can compare them and tell
if the query system is not actually making any progress.

**So here is my exercise answer**:

* Describe what kind of information is included in this history.

    We just use frames, if there is an infinite loop,
    the frame will keep growing and repeating itself,
    this can be considered a history.

* How the check should be made.

    By the observation I made above, we will achieve the goal by:

    * detect binding groups by examining the frame

    To detect the binding group, we just take the first variable in the frame,
    and see if we can find the same variable (with embeded number minused by 1)
    in the rest of the frame. This step finds the binding group length as well.
    (We don't care if other variables are corresponding, as it will eventually
    be taken into account)

    * take the first two (newest) binding group if found

    After the length of the binding group is found, we take the first two binding groups
    if found.

    * modify recursively on the first binding group and then compare

    And we apply "minus id by 1" on every variable / value of the first binding group.
    (e.g. `(? 2 x)` will become `(? 1 x)`)
    If the resulting binding group is exactly the same as the second binding group,
    then a loop is detected.
