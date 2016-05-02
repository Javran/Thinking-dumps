The exercise wants us to prove that: the right spine of a leftist heap of size $n$
contains at most $\lfloor\log(n+1)\rfloor$ elements.

The proof is inspired by: [Leftist Heap slides by Brian Curless](http://courses.cs.washington.edu/courses/cse326/08sp/lectures/05-leftist-heaps.pdf)

We first proof a lemma: if the right spine has $r$ nodes,
then the tree has at least $2^r-1$ nodes.

We can prove this by induction on $r$.

**Base case**: $r = 1$

The tree should have at least $2^r-1 = 1$ nodes, which is true.

**Induction step**: assume when $r = k$, the tree has at least $2^k-1$ nodes,
we want to show that when $r = k+1$, the tree has at least $2^{k+1}-1$ nodes.

An observation about leftist heap is: its both subtrees should also be
valid leftist heaps. For a leftist heap whose right spine has $k+1$ nodes,
its right subtree must be one leftist heap that has $k$ nodes.
And this further constrains its left sibling to also be a leftist heap that
has at least $k$ nodes. (otherwise the root's left child node's rank will be less than
that of the right child's, which violates the leftist property).

Therefore, by assumption we know the right subtree has at least $2^k-1$ nodes.
and also the left subtree has at least $2^k-1$ nodes.

Thus the whole tree has $(2^k-1) + 1 + (2^k-1) = 2^{k+1}-1$ nodes.

To summarize, we have shown that if the right spine of a leftist heap has $r$ nodes,
then it contains at least $2^r-1$ nodes.

With this lemma in mind, we can prove the original statement by contradiction.

Assume there is a leftist heap that has $n$ elements, with its right spine containing $r'$ elements.
Such that $r' > \lfloor \log(n+1) \rfloor$.

By the lemma we have proved above, we can conclude that $n \ge 2^{r'}-1$.

Notice that:

* $n \ge 2^{r'}-1$
* $n+1 \ge 2^{r'}$
* $\log(n+1) \ge r'$

Putting these two bounds on $r'$ together, we have:

* $\lfloor \log(n+1) \rfloor < r' \le \log(n+1)$

Because $r'$ must be an integer, there is no solution to it.
Therefore we have shown that a leftist heap of size $n$ cannot contain
more than $\lfloor \log(n+1) \rfloor$ elements.
