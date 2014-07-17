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

