(** * MoreCoq: More About Coq *)

Require Export Poly.

(** This chapter introduces several more Coq tactics that,
    together, allow us to prove many more theorems about the
    functional programs we are writing. *)

(* ###################################################### *)
(** * The [apply] Tactic *)

(** We often encounter situations where the goal to be proved is
    exactly the same as some hypothesis in the context or some
    previously proved lemma. *)

Theorem silly1 : forall (n m o p : nat),
     n = m  ->
     [n;o] = [n;p] ->
     [n;o] = [m;p].
Proof.
  intros n m o p eq1 eq2.
  rewrite <- eq1.
  (* At this point, we could finish with
     "[rewrite -> eq2. reflexivity.]" as we have
     done several times above. But we can achieve the
     same effect in a single step by using the
     [apply] tactic instead: *)
  apply eq2.  Qed.

(** The [apply] tactic also works with _conditional_ hypotheses
    and lemmas: if the statement being applied is an implication, then
    the premises of this implication will be added to the list of
    subgoals needing to be proved. *)

Theorem silly2 : forall (n m o p : nat),
     n = m  ->
     (forall (q r : nat), q = r -> [q;o] = [r;p]) ->
     [n;o] = [m;p].
Proof.
  intros n m o p eq1 eq2.
  apply eq2. apply eq1.  Abort.


(** You may find it instructive to experiment with this proof
    and see if there is a way to complete it using just [rewrite]
    instead of [apply]. *)

Theorem silly2 : forall (n m o p : nat),
     n = m  ->
     (forall (q r : nat), q = r -> [q;o] = [r;p]) ->
     [n;o] = [m;p].
Proof.
  intros n m o p eq1 eq2.
  assert (n = m -> [n;o] = [m;p]).
    rewrite (eq2 n m). reflexivity.
    rewrite eq1. reflexivity.
  rewrite (H eq1). reflexivity.
Qed.

(** Typically, when we use [apply H], the statement [H] will
    begin with a [forall] binding some _universal variables_.  When
    Coq matches the current goal against the conclusion of [H], it
    will try to find appropriate values for these variables.  For
    example, when we do [apply eq2] in the following proof, the
    universal variable [q] in [eq2] gets instantiated with [n] and [r]
    gets instantiated with [m]. *)

Theorem silly2a : forall (n m : nat),
     (n,n) = (m,m)  ->
     (forall (q r : nat), (q,q) = (r,r) -> [q] = [r]) ->
     [n] = [m].
Proof.
  intros n m eq1 eq2.
  apply eq2. apply eq1.  Qed.

(** **** Exercise: 2 stars, optional (silly_ex) *)
(** Complete the following proof without using [simpl]. *)

Theorem silly_ex :
     (forall n, evenb n = true -> oddb (S n) = true) ->
     evenb 3 = true ->
     oddb 4 = true.
Proof.
  intros H. apply H.
Qed.
(** [] *)

(** To use the [apply] tactic, the (conclusion of the) fact
    being applied must match the goal _exactly_ -- for example, [apply]
    will not work if the left and right sides of the equality are
    swapped. *)

Theorem silly3_firsttry : forall (n : nat),
     true = beq_nat n 5  ->
     beq_nat (S (S n)) 7 = true.
Proof.
  intros n H.
  simpl.
  (* Here we cannot use [apply] directly *)
Abort.

(** In this case we can use the [symmetry] tactic, which switches the
    left and right sides of an equality in the goal. *)

Theorem silly3 : forall (n : nat),
     true = beq_nat n 5  ->
     beq_nat (S (S n)) 7 = true.
Proof.
  intros n H.
  symmetry.
  simpl. (* Actually, this [simpl] is unnecessary, since
            [apply] will do a [simpl] step first. *)
  apply H.  Qed.

(** **** Exercise: 3 stars (apply_exercise1) *)
(** Hint: you can use [apply] with previously defined lemmas, not
    just hypotheses in the context.  Remember that [SearchAbout] is
    your friend. *)

Theorem rev_exercise1 : forall (l l' : list nat),
     l = rev l' ->
     l' = rev l.
Proof.
  intros l l' H. rewrite H.
  symmetry. apply rev_involutive.
Qed.
(** [] *)

(** **** Exercise: 1 star, optional (apply_rewrite) *)
(** Briefly explain the difference between the tactics [apply] and
    [rewrite].  Are there situations where both can usefully be
    applied?

 - apply: trys to simplify first, and applies hypothesis / lemma to the goal.
     requires exact match. might need to prove preconditions afterwards.
 - rewrite: can rewrite using a hypothesis / lemma both from left to right
     and from right to left. it can be use to rewrite only a portion of the conclusion
     does less thing than "apply".

*)
(** [] *)


(* ###################################################### *)
(** * The [apply ... with ...] Tactic *)

(** The following silly example uses two rewrites in a row to
    get from [[a,b]] to [[e,f]]. *)

Example trans_eq_example : forall (a b c d e f : nat),
     [a;b] = [c;d] ->
     [c;d] = [e;f] ->
     [a;b] = [e;f].
Proof.
  intros a b c d e f eq1 eq2.
  rewrite -> eq1. rewrite -> eq2. reflexivity.  Qed.

(** Since this is a common pattern, we might
    abstract it out as a lemma recording once and for all
    the fact that equality is transitive. *)

Theorem trans_eq : forall (X:Type) (n m o : X),
  n = m -> m = o -> n = o.
Proof.
  intros X n m o eq1 eq2. rewrite -> eq1. rewrite -> eq2.
  reflexivity.  Qed.

(** Now, we should be able to use [trans_eq] to
    prove the above example.  However, to do this we need
    a slight refinement of the [apply] tactic. *)

Example trans_eq_example' : forall (a b c d e f : nat),
     [a;b] = [c;d] ->
     [c;d] = [e;f] ->
     [a;b] = [e;f].
Proof.
  intros a b c d e f eq1 eq2.
  (* If we simply tell Coq [apply trans_eq] at this point,
     it can tell (by matching the goal against the
     conclusion of the lemma) that it should instantiate [X]
     with [[nat]], [n] with [[a,b]], and [o] with [[e,f]].
     However, the matching process doesn't determine an
     instantiation for [m]: we have to supply one explicitly
     by adding [with (m:=[c,d])] to the invocation of
     [apply]. *)
  apply trans_eq with (m:=[c;d]). apply eq1. apply eq2.   Qed.

(**  Actually, we usually don't have to include the name [m]
    in the [with] clause; Coq is often smart enough to
    figure out which instantiation we're giving. We could
    instead write: [apply trans_eq with [c,d]]. *)

(** **** Exercise: 3 stars, optional (apply_with_exercise) *)
Example trans_eq_exercise : forall (n m o p : nat),
     m = (minustwo o) ->
     (n + p) = m ->
     (n + p) = (minustwo o).
Proof.
  intros n m o p eq1 eq2.
  apply trans_eq with m. apply eq2. apply eq1.
Qed.
(** [] *)


(* ###################################################### *)
(** * The [inversion] tactic *)

(** Recall the definition of natural numbers:
     Inductive nat : Type :=
       | O : nat
       | S : nat -> nat.
    It is clear from this definition that every number has one of two
    forms: either it is the constructor [O] or it is built by applying
    the constructor [S] to another number.  But there is more here than
    meets the eye: implicit in the definition (and in our informal
    understanding of how datatype declarations work in other
    programming languages) are two other facts:

    - The constructor [S] is _injective_.  That is, the only way we can
      have [S n = S m] is if [n = m].

    - The constructors [O] and [S] are _disjoint_.  That is, [O] is not
      equal to [S n] for any [n]. *)

(** Similar principles apply to all inductively defined types: all
    constructors are injective, and the values built from distinct
    constructors are never equal.  For lists, the [cons] constructor is
    injective and [nil] is different from every non-empty list.  For
    booleans, [true] and [false] are unequal.  (Since neither [true]
    nor [false] take any arguments, their injectivity is not an issue.) *)

(** Coq provides a tactic called [inversion] that allows us to exploit
    these principles in proofs.

    The [inversion] tactic is used like this.  Suppose [H] is a
    hypothesis in the context (or a previously proven lemma) of the
    form
      c a1 a2 ... an = d b1 b2 ... bm
    for some constructors [c] and [d] and arguments [a1 ... an] and
    [b1 ... bm].  Then [inversion H] instructs Coq to "invert" this
    equality to extract the information it contains about these terms:

    - If [c] and [d] are the same constructor, then we know, by the
      injectivity of this constructor, that [a1 = b1], [a2 = b2],
      etc.; [inversion H] adds these facts to the context, and tries
      to use them to rewrite the goal.

    - If [c] and [d] are different constructors, then the hypothesis
      [H] is contradictory.  That is, a false assumption has crept
      into the context, and this means that any goal whatsoever is
      provable!  In this case, [inversion H] marks the current goal as
      completed and pops it off the goal stack. *)

(** The [inversion] tactic is probably easier to understand by
    seeing it in action than from general descriptions like the above.
    Below you will find example theorems that demonstrate the use of
    [inversion] and exercises to test your understanding. *)

Theorem eq_add_S : forall (n m : nat),
     S n = S m ->
     n = m.
Proof.
  intros n m eq. inversion eq. reflexivity.  Qed.

Theorem silly4 : forall (n m : nat),
     [n] = [m] ->
     n = m.
Proof.
  intros n o eq. inversion eq. reflexivity.  Qed.

(** As a convenience, the [inversion] tactic can also
    destruct equalities between complex values, binding
    multiple variables as it goes. *)

Theorem silly5 : forall (n m o : nat),
     [n;m] = [o;o] ->
     [n] = [m].
Proof.
  intros n m o eq. inversion eq. reflexivity. Qed.

(** **** Exercise: 1 star (sillyex1) *)
Example sillyex1 : forall (X : Type) (x y z : X) (l j : list X),
     x :: y :: l = z :: j ->
     y :: l = x :: j ->
     x = y.
Proof.
  intros X x y z i j eq1 eq2. inversion eq2. reflexivity.
Qed.
(** [] *)

Theorem silly6 : forall (n : nat),
     S n = O ->
     2 + 2 = 5.
Proof.
  intros n contra. inversion contra. Qed.

Theorem silly7 : forall (n m : nat),
     false = true ->
     [n] = [m].
Proof.
  intros n m contra. inversion contra.  Qed.

(** **** Exercise: 1 star (sillyex2) *)
Example sillyex2 : forall (X : Type) (x y z : X) (l j : list X),
     x :: y :: l = [] ->
     y :: l = z :: j ->
     x = z.
Proof.
  intros X x y z i j eq1 eq2. inversion eq1. Qed.
(** [] *)

(** While the injectivity of constructors allows us to reason
    [forall (n m : nat), S n = S m -> n = m], the reverse direction of
    the implication is an instance of a more general fact about
    constructors and functions, which we will often find useful: *)

Theorem f_equal : forall (A B : Type) (f: A -> B) (x y: A),
    x = y -> f x = f y.
Proof. intros A B f x y eq. rewrite eq.  reflexivity.  Qed.

(** Here's another illustration of [inversion].  This is a slightly
    roundabout way of stating a fact that we have already proved
    above.  The extra equalities force us to do a little more
    equational reasoning and exercise some of the tactics we've seen
    recently. *)

Theorem length_snoc' : forall (X : Type) (v : X)
                              (l : list X) (n : nat),
     length l = n ->
     length (snoc l v) = S n.
Proof.
  intros X v l. induction l as [| v' l'].
  Case "l = []". intros n eq. rewrite <- eq. reflexivity.
  Case "l = v' :: l'". intros n eq. simpl. destruct n as [| n'].
    SCase "n = 0". inversion eq.
    SCase "n = S n'".
      apply f_equal. apply IHl'. inversion eq. reflexivity. Qed.


(** **** Exercise: 2 stars, optional (practice) *)
(** A couple more nontrivial but not-too-complicated proofs to work
    together in class, or for you to work as exercises.  They may
    involve applying lemmas from earlier lectures or homeworks. *)


Theorem beq_nat_0_l : forall n,
   beq_nat 0 n = true -> n = 0.
Proof.
  intros n eq1. destruct n as [|n'].
  Case "n = 0". reflexivity.
  Case "n = S n'". inversion eq1.
Qed.

Theorem beq_nat_0_r : forall n,
   beq_nat n 0 = true -> n = 0.
Proof.
  intros n eq1. destruct n as [|n'].
  Case "n = 0". reflexivity.
  Case "n = S n'". inversion eq1.
Qed.
(** [] *)


(* ###################################################### *)
(** * Using Tactics on Hypotheses *)

(** By default, most tactics work on the goal formula and leave
    the context unchanged.  However, most tactics also have a variant
    that performs a similar operation on a statement in the context.

    For example, the tactic [simpl in H] performs simplification in
    the hypothesis named [H] in the context. *)

Theorem S_inj : forall (n m : nat) (b : bool),
     beq_nat (S n) (S m) = b  ->
     beq_nat n m = b.
Proof.
  intros n m b H. simpl in H. apply H.  Qed.

(** Similarly, the tactic [apply L in H] matches some
    conditional statement [L] (of the form [L1 -> L2], say) against a
    hypothesis [H] in the context.  However, unlike ordinary
    [apply] (which rewrites a goal matching [L2] into a subgoal [L1]),
    [apply L in H] matches [H] against [L1] and, if successful,
    replaces it with [L2].

    In other words, [apply L in H] gives us a form of "forward
    reasoning" -- from [L1 -> L2] and a hypothesis matching [L1], it
    gives us a hypothesis matching [L2].  By contrast, [apply L] is
    "backward reasoning" -- it says that if we know [L1->L2] and we
    are trying to prove [L2], it suffices to prove [L1].

    Here is a variant of a proof from above, using forward reasoning
    throughout instead of backward reasoning. *)

Theorem silly3' : forall (n : nat),
  (beq_nat n 5 = true -> beq_nat (S (S n)) 7 = true) ->
     true = beq_nat n 5  ->
     true = beq_nat (S (S n)) 7.
Proof.
  intros n eq H.
  symmetry in H. apply eq in H. symmetry in H.
  apply H.  Qed.

(** Forward reasoning starts from what is _given_ (premises,
    previously proven theorems) and iteratively draws conclusions from
    them until the goal is reached.  Backward reasoning starts from
    the _goal_, and iteratively reasons about what would imply the
    goal, until premises or previously proven theorems are reached.
    If you've seen informal proofs before (for example, in a math or
    computer science class), they probably used forward reasoning.  In
    general, Coq tends to favor backward reasoning, but in some
    situations the forward style can be easier to use or to think
    about.  *)

(** **** Exercise: 3 stars (plus_n_n_injective) *)
(** Practice using "in" variants in this exercise. *)

Theorem plus_n_n_injective : forall n m,
     n + n = m + m ->
     n = m.
Proof.
  intros n. induction n as [| n'].
  Case "n = 0". intros m eq1. simpl in eq1. rewrite eq1.
    induction m as [| m'].
    SCase "m = 0". reflexivity.
    SCase "m = S m'". rewrite <- plus_n_Sm in eq1. inversion eq1.
  Case "n = S n'". intros m eq2.
  rewrite <- plus_n_Sm in eq2.
    destruct m as [| m'].
    SCase "m = 0". simpl in eq2. inversion eq2.
    SCase "m = S m'". simpl in eq2.
      assert (S (m' + S m') = S (S (m' + m'))).
        rewrite <- plus_n_Sm. reflexivity.
      rewrite H in eq2. inversion eq2.
      apply f_equal. apply IHn'.  apply H1.
Qed.
(** [] *)

(* ###################################################### *)
(** * Varying the Induction Hypothesis *)

(** Sometimes it is important to control the exact form of the
    induction hypothesis when carrying out inductive proofs in Coq.
    In particular, we need to be careful about which of the
    assumptions we move (using [intros]) from the goal to the context
    before invoking the [induction] tactic.  For example, suppose
    we want to show that the [double] function is injective -- i.e.,
    that it always maps different arguments to different results:
    Theorem double_injective: forall n m, double n = double m -> n = m.
    The way we _start_ this proof is a little bit delicate: if we
    begin it with
      intros n. induction n.
]]
    all is well.  But if we begin it with
      intros n m. induction n.
    we get stuck in the middle of the inductive case... *)

Theorem double_injective_FAILED : forall n m,
     double n = double m ->
     n = m.
Proof.
  intros n m. induction n as [| n'].
  Case "n = O". simpl. intros eq. destruct m as [| m'].
    SCase "m = O". reflexivity.
    SCase "m = S m'". inversion eq.
  Case "n = S n'". intros eq. destruct m as [| m'].
    SCase "m = O". inversion eq.
    SCase "m = S m'".  apply f_equal.
      (* Here we are stuck.  The induction hypothesis, [IHn'], does
         not give us [n' = m'] -- there is an extra [S] in the
         way -- so the goal is not provable. *)
      Abort.

(** What went wrong? *)

(** The problem is that, at the point we invoke the induction
    hypothesis, we have already introduced [m] into the context --
    intuitively, we have told Coq, "Let's consider some particular
    [n] and [m]..." and we now have to prove that, if [double n =
    double m] for _this particular_ [n] and [m], then [n = m].

    The next tactic, [induction n] says to Coq: We are going to show
    the goal by induction on [n].  That is, we are going to prove that
    the proposition

      - [P n]  =  "if [double n = double m], then [n = m]"

    holds for all [n] by showing

      - [P O]

         (i.e., "if [double O = double m] then [O = m]")

      - [P n -> P (S n)]

        (i.e., "if [double n = double m] then [n = m]" implies "if
        [double (S n) = double m] then [S n = m]").

    If we look closely at the second statement, it is saying something
    rather strange: it says that, for a _particular_ [m], if we know

      - "if [double n = double m] then [n = m]"

    then we can prove

       - "if [double (S n) = double m] then [S n = m]".

    To see why this is strange, let's think of a particular [m] --
    say, [5].  The statement is then saying that, if we know

      - [Q] = "if [double n = 10] then [n = 5]"

    then we can prove

      - [R] = "if [double (S n) = 10] then [S n = 5]".

    But knowing [Q] doesn't give us any help with proving [R]!  (If we
    tried to prove [R] from [Q], we would say something like "Suppose
    [double (S n) = 10]..." but then we'd be stuck: knowing that
    [double (S n)] is [10] tells us nothing about whether [double n]
    is [10], so [Q] is useless at this point.) *)

(** To summarize: Trying to carry out this proof by induction on [n]
    when [m] is already in the context doesn't work because we are
    trying to prove a relation involving _every_ [n] but just a
    _single_ [m]. *)

(** The good proof of [double_injective] leaves [m] in the goal
    statement at the point where the [induction] tactic is invoked on
    [n]: *)

Theorem double_injective : forall n m,
     double n = double m ->
     n = m.
Proof.
  intros n. induction n as [| n'].
  Case "n = O". simpl. intros m eq. destruct m as [| m'].
    SCase "m = O". reflexivity.
    SCase "m = S m'". inversion eq.
  Case "n = S n'".
    (* Notice that both the goal and the induction
       hypothesis have changed: the goal asks us to prove
       something more general (i.e., to prove the
       statement for _every_ [m]), but the IH is
       correspondingly more flexible, allowing us to
       choose any [m] we like when we apply the IH.  *)
    intros m eq.
    (* Now we choose a particular [m] and introduce the
       assumption that [double n = double m].  Since we
       are doing a case analysis on [n], we need a case
       analysis on [m] to keep the two "in sync." *)
    destruct m as [| m'].
    SCase "m = O".
      (* The 0 case is trivial *)
      inversion eq.
    SCase "m = S m'".
      apply f_equal.
      (* At this point, since we are in the second
         branch of the [destruct m], the [m'] mentioned
         in the context at this point is actually the
         predecessor of the one we started out talking
         about.  Since we are also in the [S] branch of
         the induction, this is perfect: if we
         instantiate the generic [m] in the IH with the
         [m'] that we are talking about right now (this
         instantiation is performed automatically by
         [apply]), then [IHn'] gives us exactly what we
         need to finish the proof. *)
      apply IHn'. inversion eq. reflexivity. Qed.

(** What this teaches us is that we need to be careful about using
    induction to try to prove something too specific: If we're proving
    a property of [n] and [m] by induction on [n], we may need to
    leave [m] generic. *)

(** The proof of this theorem has to be treated similarly: *)

(** **** Exercise: 2 stars (beq_nat_true) *)
Theorem beq_nat_true : forall n m,
    beq_nat n m = true -> n = m.
Proof.
  intros n. induction n as [|n'].
  Case "n = 0". symmetry. apply beq_nat_0_l. apply H.
  Case "n = S n'". intros m eq1. destruct m as [|m'].
    SCase "m = 0". inversion eq1.
    SCase "m = S m'". apply f_equal. apply IHn'.
      simpl in eq1. apply eq1.
Qed.
(** [] *)

(** **** Exercise: 2 stars, advanced (beq_nat_true_informal) *)
(** Give a careful informal proof of [beq_nat_true], being as explicit
    as possible about quantifiers. *)
(*
  We want to prove: suppose m n are two natural numbers, if [beq_nat n m]
  returns [true], then [m = n].

  We will prove it by induction on n.

  - Base case: n = 0.
    Since we know that for any given natural number l, if [beq_nat 0 l = true],
    then [l = 0]. The conclusion follows by letting [m = l], and we have [m = 0 = n].
  - Inductive step: n = S n'
    Assume for any given n' < n, the conclusion holds. We need to prove
    if [beq_nat (S n') m = true] then [m = S n' = n] for any natural number m.

    This can be proved by considering two cases:

    - Case #1: m = 0

    We know that if [beq_nat (S n') 0 = true], we must have [S n' = 0],
    which is impossible.

    - Case #2: m = S m' > 0

    Since [beq_nat n m = beq_nat (S n') (S m') = true], by definition, we also have:
    [beq_nat n' m' = true]. And the conclusion holds by applying the hypothesis,
    which is: since [beq_nat n' m' = true], we have [n' = m'], thus [S n' = S m']
    therefore [n = m].

*)
(** [] *)


(** The strategy of doing fewer [intros] before an [induction] doesn't
    always work directly; sometimes a little _rearrangement_ of
    quantified variables is needed.  Suppose, for example, that we
    wanted to prove [double_injective] by induction on [m] instead of
    [n]. *)

Theorem double_injective_take2_FAILED : forall n m,
     double n = double m ->
     n = m.
Proof.
  intros n m. induction m as [| m'].
  Case "m = O". simpl. intros eq. destruct n as [| n'].
    SCase "n = O". reflexivity.
    SCase "n = S n'". inversion eq.
  Case "m = S m'". intros eq. destruct n as [| n'].
    SCase "n = O". inversion eq.
    SCase "n = S n'".  apply f_equal.
        (* Stuck again here, just like before. *)
Abort.

(** The problem is that, to do induction on [m], we must first
    introduce [n].  (If we simply say [induction m] without
    introducing anything first, Coq will automatically introduce
    [n] for us!)   *)

(** What can we do about this?  One possibility is to rewrite the
    statement of the lemma so that [m] is quantified before [n].  This
    will work, but it's not nice: We don't want to have to mangle the
    statements of lemmas to fit the needs of a particular strategy for
    proving them -- we want to state them in the most clear and
    natural way. *)

(**  What we can do instead is to first introduce all the
    quantified variables and then _re-generalize_ one or more of
    them, taking them out of the context and putting them back at
    the beginning of the goal.  The [generalize dependent] tactic
    does this. *)

Theorem double_injective_take2 : forall n m,
     double n = double m ->
     n = m.
Proof.
  intros n m.
  (* [n] and [m] are both in the context *)
  generalize dependent n.
  (* Now [n] is back in the goal and we can do induction on
     [m] and get a sufficiently general IH. *)
  induction m as [| m'].
  Case "m = O". simpl. intros n eq. destruct n as [| n'].
    SCase "n = O". reflexivity.
    SCase "n = S n'". inversion eq.
  Case "m = S m'". intros n eq. destruct n as [| n'].
    SCase "n = O". inversion eq.
    SCase "n = S n'". apply f_equal.
      apply IHm'. inversion eq. reflexivity. Qed.

(** Let's look at an informal proof of this theorem.  Note that
    the proposition we prove by induction leaves [n] quantified,
    corresponding to the use of generalize dependent in our formal
    proof.

_Theorem_: For any nats [n] and [m], if [double n = double m], then
  [n = m].

_Proof_: Let [m] be a [nat]. We prove by induction on [m] that, for
  any [n], if [double n = double m] then [n = m].

  - First, suppose [m = 0], and suppose [n] is a number such
    that [double n = double m].  We must show that [n = 0].

    Since [m = 0], by the definition of [double] we have [double n =
    0].  There are two cases to consider for [n].  If [n = 0] we are
    done, since this is what we wanted to show.  Otherwise, if [n = S
    n'] for some [n'], we derive a contradiction: by the definition of
    [double] we would have [double n = S (S (double n'))], but this
    contradicts the assumption that [double n = 0].

  - Otherwise, suppose [m = S m'] and that [n] is again a number such
    that [double n = double m].  We must show that [n = S m'], with
    the induction hypothesis that for every number [s], if [double s =
    double m'] then [s = m'].

    By the fact that [m = S m'] and the definition of [double], we
    have [double n = S (S (double m'))].  There are two cases to
    consider for [n].

    If [n = 0], then by definition [double n = 0], a contradiction.
    Thus, we may assume that [n = S n'] for some [n'], and again by
    the definition of [double] we have [S (S (double n')) = S (S
    (double m'))], which implies by inversion that [double n' = double
    m'].

    Instantiating the induction hypothesis with [n'] thus allows us to
    conclude that [n' = m'], and it follows immediately that [S n' = S
    m'].  Since [S n' = n] and [S m' = m], this is just what we wanted
    to show. [] *)

(** **** Exercise: 3 stars (gen_dep_practice) *)

(** Prove this by induction on [l]. *)

Theorem index_after_last: forall (n : nat) (X : Type) (l : list X),
     length l = n ->
     index n l = None.
Proof.
  intros n X l eq1. generalize dependent n.
  induction l as [|x l'].
  Case "l = nil". reflexivity.
  Case "l = cons x l'". intros n eq1. simpl in eq1. destruct n.
    SCase "n = 0". inversion eq1.
    SCase "n = S n'". apply IHl'. inversion eq1. reflexivity.
Qed.
(** [] *)

(** **** Exercise: 3 stars, advanced, optional (index_after_last_informal) *)
(** Write an informal proof corresponding to your Coq proof
    of [index_after_last]:

     _Theorem_: For all sets [X], lists [l : list X], and numbers
      [n], if [length l = n] then [index n l = None].

     _Proof_:

     We will prove it by induction on [l].

     - Base case: l = nil

     The conclusion follows by observing that
     [length l = n = 0] and [index n l = None]

     - Inductive step: suppose l = x :: l',
       if [length l' = n'] then [index n' l' = None].

     Since [l] has at least one element, we know
     [length l = n > 0]. Let [n = S n'], by defintion of [length],
     we know [length l' = n'], therefore [index n' l' = None].
     The conclusion follows by observation that [length l = S (length (x :: l')) = n]
     and [index (S n') (x :: l') = None].

[]
*)

(** **** Exercise: 3 stars, optional (gen_dep_practice_more) *)
(** Prove this by induction on [l]. *)

Theorem length_snoc''' : forall (n : nat) (X : Type)
                              (v : X) (l : list X),
     length l = n ->
     length (snoc l v) = S n.
Proof.
  intros n X v l. generalize dependent n. induction l as [|x l'].
  Case "l = nil". intros n eq1. simpl in eq1.
    rewrite <- eq1. reflexivity.
  Case "l = cons x l'". intros n eq1. simpl in eq1. destruct n.
    SCase "n = 0". inversion eq1.
    SCase "n = S n'". inversion eq1. rewrite H0. simpl.
      apply f_equal. apply IHl'. apply H0.
Qed.
(** [] *)

(** **** Exercise: 3 stars, optional (app_length_cons) *)
(** Prove this by induction on [l1], without using [app_length]. *)

Theorem app_length_cons : forall (X : Type) (l1 l2 : list X)
                                  (x : X) (n : nat),
     length (l1 ++ (x :: l2)) = n ->
     S (length (l1 ++ l2)) = n.
Proof.
  intros X l1 l2 x n. generalize dependent n.
  induction l1 as [|x1 l1'].
  Case "l1 = nil". intros n eq1. simpl in eq1. destruct n.
    SCase "n = 0". inversion eq1.
    SCase "n = S n'". inversion eq1. apply f_equal. reflexivity.
  Case "l1 = cons x1 l1'". intros n eq1. simpl in eq1. destruct n.
    SCase "n = 0". inversion eq1.
    SCase "n = S n'". apply f_equal. apply IHl1'.
      inversion eq1. reflexivity.
Qed.
(** [] *)

(** **** Exercise: 4 stars, optional (app_length_twice) *)
(** Prove this by induction on [l], without using app_length. *)

Lemma app_nil : forall (X : Type) (l : list X),
  l ++ [] = l.
Proof.
  intros X l. induction l as [|x l'].
  Case "l = nil". reflexivity.
  Case "l = cons x l'". simpl. rewrite IHl'. reflexivity.
Qed.

Lemma app_commute_under_length : forall (X : Type) (l1 l2 : list X),
  length (l1 ++ l2) = length (l2 ++ l1).
Proof.
  intros X l1 l2. induction l1 as [|x l1'].
  Case "l1 = nil". apply f_equal. rewrite app_nil. reflexivity.
  Case "l1 = cons x l1'". simpl. rewrite IHl1'. apply app_length_cons with x.
    reflexivity.
Qed.

Theorem app_length_twice : forall (X:Type) (n:nat) (l:list X),
     length l = n ->
     length (l ++ l) = n + n.
Proof.
  intros X n l. generalize dependent n. induction l as [|x l'].
  Case "l = nil". intros n eq1. simpl in eq1.
    simpl. rewrite <- eq1. reflexivity.
  Case "l = cons x l'". intros n eq1. destruct n.
    SCase "n = 0". simpl in eq1. inversion eq1.
    SCase "n = S n'". simpl in eq1. inversion eq1.
      rewrite eq1. simpl. rewrite app_commute_under_length. simpl.
      rewrite <- plus_n_Sm. apply f_equal. apply f_equal.
      apply IHl'. apply H0.
Qed.
(** [] *)

(* ###################################################### *)
(** * Using [destruct] on Compound Expressions *)

(** We have seen many examples where the [destruct] tactic is
    used to perform case analysis of the value of some variable.  But
    sometimes we need to reason by cases on the result of some
    _expression_.  We can also do this with [destruct].

    Here are some examples: *)

Definition sillyfun (n : nat) : bool :=
  if beq_nat n 3 then false
  else if beq_nat n 5 then false
  else false.

Theorem sillyfun_false : forall (n : nat),
  sillyfun n = false.
Proof.
  intros n. unfold sillyfun.
  destruct (beq_nat n 3).
    Case "beq_nat n 3 = true". reflexivity.
    Case "beq_nat n 3 = false". destruct (beq_nat n 5).
      SCase "beq_nat n 5 = true". reflexivity.
      SCase "beq_nat n 5 = false". reflexivity.  Qed.

(** After unfolding [sillyfun] in the above proof, we find that
    we are stuck on [if (beq_nat n 3) then ... else ...].  Well,
    either [n] is equal to [3] or it isn't, so we use [destruct
    (beq_nat n 3)] to let us reason about the two cases.

    In general, the [destruct] tactic can be used to perform case
    analysis of the results of arbitrary computations.  If [e] is an
    expression whose type is some inductively defined type [T], then,
    for each constructor [c] of [T], [destruct e] generates a subgoal
    in which all occurrences of [e] (in the goal and in the context)
    are replaced by [c].

*)

(** **** Exercise: 1 star (override_shadow) *)
Theorem override_shadow : forall (X:Type) x1 x2 k1 k2 (f : nat->X),
  (override (override f k1 x2) k1 x1) k2 = (override f k1 x1) k2.
Proof.
  intros X x1 x2 k1 k2 f. unfold override.
  destruct (beq_nat k1 k2).
  Case "beq_nat k1 k2 = true". reflexivity.
  Case "beq_nat k1 k2 = false". reflexivity.
Qed.
(** [] *)

(** **** Exercise: 3 stars, optional (combine_split) *)
(** Complete the proof below *)

Lemma pair_fst_snd : forall (X Y : Type) (p : X * Y),
  p = (fst p, snd p).
Proof.
  intros X Y p. destruct p. reflexivity.
Qed.

Theorem combine_split : forall X Y (l : list (X * Y)) l1 l2,
  split l = (l1, l2) ->
  combine l1 l2 = l.
Proof.
  intros X Y l. induction l as [|(x,y) l'].
  Case "l = nil". intros l1 l2 eq1.
    simpl in eq1. inversion eq1. reflexivity.
  Case "l = (x,y) :: l'". intros l1 l2 eq1. unfold combine.
    inversion eq1. apply f_equal. apply IHl'.
    apply pair_fst_snd.
Qed.
(** [] *)

(** Sometimes, doing a [destruct] on a compound expression (a
    non-variable) will erase information we need to complete a proof. *)
(** For example, suppose
    we define a function [sillyfun1] like this: *)

Definition sillyfun1 (n : nat) : bool :=
  if beq_nat n 3 then true
  else if beq_nat n 5 then true
  else false.

(** And suppose that we want to convince Coq of the rather
    obvious observation that [sillyfun1 n] yields [true] only when [n]
    is odd.  By analogy with the proofs we did with [sillyfun] above,
    it is natural to start the proof like this: *)

Theorem sillyfun1_odd_FAILED : forall (n : nat),
     sillyfun1 n = true ->
     oddb n = true.
Proof.
  intros n eq. unfold sillyfun1 in eq.
  destruct (beq_nat n 3).
  (* stuck... *)
Abort.

(** We get stuck at this point because the context does not
    contain enough information to prove the goal!  The problem is that
    the substitution peformed by [destruct] is too brutal -- it threw
    away every occurrence of [beq_nat n 3], but we need to keep some
    memory of this expression and how it was destructed, because we
    need to be able to reason that since, in this branch of the case
    analysis, [beq_nat n 3 = true], it must be that [n = 3], from
    which it follows that [n] is odd.

    What we would really like is to substitute away all existing
    occurences of [beq_nat n 3], but at the same time add an equation
    to the context that records which case we are in.  The [eqn:]
    qualifier allows us to introduce such an equation (with whatever
    name we choose). *)

Theorem sillyfun1_odd : forall (n : nat),
     sillyfun1 n = true ->
     oddb n = true.
Proof.
  intros n eq. unfold sillyfun1 in eq.
  destruct (beq_nat n 3) eqn:Heqe3.
  (* Now we have the same state as at the point where we got stuck
    above, except that the context contains an extra equality
    assumption, which is exactly what we need to make progress. *)
    Case "e3 = true". apply beq_nat_true in Heqe3.
      rewrite -> Heqe3. reflexivity.
    Case "e3 = false".
     (* When we come to the second equality test in the body of the
       function we are reasoning about, we can use [eqn:] again in the
       same way, allow us to finish the proof. *)
      destruct (beq_nat n 5) eqn:Heqe5.
        SCase "e5 = true".
          apply beq_nat_true in Heqe5.
          rewrite -> Heqe5. reflexivity.
        SCase "e5 = false". inversion eq.  Qed.


(** **** Exercise: 2 stars (destruct_eqn_practice) *)
Theorem bool_fn_applied_thrice :
  forall (f : bool -> bool) (b : bool),
  f (f (f b)) = f b.
Proof.
  intros f b. destruct (f b) eqn:Hfb.
  Case "f b = true". destruct b eqn:Hb.
    SCase "b = true". rewrite Hfb. apply Hfb.
    SCase "b = false". destruct (f true) eqn:Hfb'.
      SSCase "f true = true". apply Hfb'.
      SSCase "f true = false". apply Hfb.
  Case "f b = false". destruct b eqn:Hb.
    SCase "b = true". destruct (f false) eqn:Hfb'.
      SSCase "f false = true". apply Hfb.
      SSCase "f false = false". apply Hfb'.
    SCase "b = false". rewrite Hfb. apply Hfb.
Qed.
(** [] *)

(** **** Exercise: 2 stars (override_same) *)
Theorem override_same : forall (X:Type) x1 k1 k2 (f : nat->X),
  f k1 = x1 ->
  (override f k1 x1) k2 = f k2.
Proof.
  intros X x1 k1 k2 f eq1. unfold override.
  destruct (beq_nat k1 k2) eqn:Heq12.
  Case "beq_nat k1 k2 = true".
    assert (k1 = k2). apply beq_nat_true. apply Heq12.
    rewrite <- H. symmetry. apply eq1.
  Case "beq_nat k1 k2 = false". reflexivity.
Qed.
(** [] *)

(* ################################################################## *)
(** * Review *)

(** We've now seen a bunch of Coq's fundamental tactics.  We'll
    introduce a few more as we go along through the coming lectures,
    and later in the course we'll introduce some more powerful
    _automation_ tactics that make Coq do more of the low-level work
    in many cases.  But basically we've got what we need to get work
    done.

    Here are the ones we've seen:

      - [intros]:
        move hypotheses/variables from goal to context

      - [reflexivity]:
        finish the proof (when the goal looks like [e = e])

      - [apply]:
        prove goal using a hypothesis, lemma, or constructor

      - [apply... in H]:
        apply a hypothesis, lemma, or constructor to a hypothesis in
        the context (forward reasoning)

      - [apply... with...]:
        explicitly specify values for variables that cannot be
        determined by pattern matching

      - [simpl]:
        simplify computations in the goal

      - [simpl in H]:
        ... or a hypothesis

      - [rewrite]:
        use an equality hypothesis (or lemma) to rewrite the goal

      - [rewrite ... in H]:
        ... or a hypothesis

      - [symmetry]:
        changes a goal of the form [t=u] into [u=t]

      - [symmetry in H]:
        changes a hypothesis of the form [t=u] into [u=t]

      - [unfold]:
        replace a defined constant by its right-hand side in the goal

      - [unfold... in H]:
        ... or a hypothesis

      - [destruct... as...]:
        case analysis on values of inductively defined types

      - [destruct... eqn:...]:
        specify the name of an equation to be added to the context,
        recording the result of the case analysis

      - [induction... as...]:
        induction on values of inductively defined types

      - [inversion]:
        reason by injectivity and distinctness of constructors

      - [assert (e) as H]:
        introduce a "local lemma" [e] and call it [H]

      - [generalize dependent x]:
        move the variable [x] (and anything else that depends on it)
        from the context back to an explicit hypothesis in the goal
        formula
*)

(* ###################################################### *)
(** * Additional Exercises *)

(** **** Exercise: 3 stars (beq_nat_sym) *)
Theorem beq_nat_sym : forall (n m : nat),
  beq_nat n m = beq_nat m n.
Proof.
  intros n. induction n as [|n'].
  Case "n = 0". intros m. simpl. destruct m as [|m'].
    SCase "m = 0". reflexivity.
    SCase "m = S m'". reflexivity.
  Case "n = S n'". intros m. simpl. destruct m as [|m'].
    SCase "m = 0". reflexivity.
    SCase "m = S m'". simpl. apply IHn'.
Qed.
(** [] *)

(** **** Exercise: 3 stars, advanced, optional (beq_nat_sym_informal) *)
(** Give an informal proof of this lemma that corresponds to your
    formal proof above:

   Theorem: For any [nat]s [n] [m], [beq_nat n m = beq_nat m n].

   Proof:
   We prove it by induction on [n].

   - Base case: n = 0.

     - if m = 0, then beq_nat m n = true = beq_nat n m.
     - if m > 0, then beq_nat m n = false = beq_nat n m.

   - Inductive step: n = S n'.

     The hypothesis is: for any natural number [m], we have:
     [beq_nat n' m = beq_nat m n'].

     - if m = 0, then [beq_nat (S n') m = false = beq_nat m (S n')].
     - if m = S m' > 0, we need to prove
       [beq_nat (S n') (S m') = beq_nat n' m' = beq_nat m' n' = beq_nat (S m') (S n')].
       This follows by applying the hypothesis.

    Therefore for any natural number [n] and [m], we have [beq_nat n m = beq_nat m n].

[]
 *)

(** **** Exercise: 3 stars, optional (beq_nat_trans) *)
Lemma beq_nat_n_n : forall n,
  beq_nat n n = true.
Proof.
  intros n. induction n as [|n'].
  Case "n = 0". reflexivity.
  Case "n = S n'". simpl. apply IHn'.
Qed.

Theorem beq_nat_trans : forall n m p,
  beq_nat n m = true ->
  beq_nat m p = true ->
  beq_nat n p = true.
Proof.
  intros n m p Hnm Hmp.
  assert (n = m). apply beq_nat_true. apply Hnm.
  assert (m = p). apply beq_nat_true. apply Hmp.
  assert (n = p). apply trans_eq with m. apply H.
  apply H0. rewrite H1. apply beq_nat_n_n.
Qed.
(** [] *)

(** **** Exercise: 3 stars, advanced (split_combine) *)
(** We have just proven that for all lists of pairs, [combine] is the
    inverse of [split].  How would you formalize the statement that
    [split] is the inverse of [combine]?

    Complete the definition of [split_combine_statement] below with a
    property that states that [split] is the inverse of
    [combine]. Then, prove that the property holds. (Be sure to leave
    your induction hypothesis general by not doing [intros] on more
    things than necessary.  Hint: what property do you need of [l1]
    and [l2] for [split] [combine l1 l2 = (l1,l2)] to be true?)  *)

Definition split_combine_statement : Prop :=
  forall (X : Type) (Y : Type)
         (l1 : list X) (l2 : list Y) (l : list (X * Y)),
      length l1 = length l
   -> length l2 = length l
   -> combine l1 l2 = l
   -> split l = (l1,l2).

Theorem split_combine : split_combine_statement.
Proof.
  intros X Y l1. induction l1 as [|x1 l1'].
  Case "l1 = nil". intros l2. destruct l2 as [|x2 l2'].
    SCase "l2 = nil". intros l eq1 eq2 eq3. simpl in eq3. rewrite <- eq3.
      reflexivity.
    SCase "l2 = x2 :: l2'". intros l eq1 eq2 eq3.
      simpl in eq3. rewrite <- eq3 in eq2. inversion eq2.
  Case "l1 = x1 l1'". intros l2. destruct l2 as [|x2 l2'].
    SCase "l2 = nil". intros l eq1 eq2 eq3.
      simpl in eq3. rewrite <- eq3 in eq1. inversion eq1.
    SCase "l2 = x2 :: l2". intros l eq1 eq2 eq3.
      simpl in eq3. rewrite <- eq3. destruct l as [|(a,b) l'].
      SSCase "l = nil". simpl in eq1. inversion eq1.
      SSCase "l = (a,b) :: l'".
        simpl in eq1. inversion eq1. simpl in eq2. inversion eq2.
        inversion eq3. simpl.
        assert (split (combine l1' l2') = (l1',l2')) as IH.
          apply IHl1'. rewrite H4. apply H0. rewrite H4. apply H1.
          reflexivity.
        rewrite IH. reflexivity.
Qed.
(** [] *)

(** **** Exercise: 3 stars (override_permute) *)
Theorem override_permute : forall (X:Type) x1 x2 k1 k2 k3 (f : nat->X),
  beq_nat k2 k1 = false ->
  (override (override f k2 x2) k1 x1) k3 = (override (override f k1 x1) k2 x2) k3.
Proof.
  intros X x1 x2 k1 k2 k3 f eq1.
  unfold override. destruct (beq_nat k1 k3) eqn:H13.
  Case "beq_nat k1 k3 = true".
    assert (k1 = k3). apply beq_nat_true. apply H13.
    rewrite <- H. rewrite eq1. reflexivity.
  Case "beq_nat k1 k3 = false". reflexivity.
Qed.
(** [] *)

(** **** Exercise: 3 stars, advanced (filter_exercise) *)
(** This one is a bit challenging.  Pay attention to the form of your IH. *)

Theorem filter_exercise : forall (X : Type) (test : X -> bool)
                             (x : X) (l lf : list X),
     filter test l = x :: lf ->
     test x = true.
Proof.
  intros X test x l lf.
  generalize dependent lf. generalize dependent x.
  induction l as [|a l'].
  Case "l = nil". intros x lf eq1.
    unfold filter in eq1. inversion eq1.
  Case "l = a :: l'". destruct (test a) eqn:Htx.
    SCase "test a = true".
      intros x lf eq1. unfold filter in eq1.
      rewrite Htx in eq1. inversion eq1.
      rewrite <- H0. apply Htx.
    SCase "test a = false". intros x lf eq1.
      apply IHl' with lf. unfold filter in eq1.
      rewrite Htx in eq1. apply eq1.
Qed.
(** [] *)

(** **** Exercise: 4 stars, advanced (forall_exists_challenge) *)
(** Define two recursive [Fixpoints], [forallb] and [existsb].  The
    first checks whether every element in a list satisfies a given
    predicate:
      forallb oddb [1;3;5;7;9] = true

      forallb negb [false;false] = true

      forallb evenb [0;2;4;5] = false

      forallb (beq_nat 5) [] = true
    The second checks whether there exists an element in the list that
    satisfies a given predicate:
      existsb (beq_nat 5) [0;2;3;6] = false

      existsb (andb true) [true;true;false] = true

      existsb oddb [1;0;0;0;0;3] = true

      existsb evenb [] = false
    Next, define a _nonrecursive_ version of [existsb] -- call it
    [existsb'] -- using [forallb] and [negb].

    Prove that [existsb'] and [existsb] have the same behavior.
*)

Fixpoint forallb {X : Type} (f : X -> bool) (xs : list X) : bool :=
  match xs with
  | [] => true
  | h :: t => if f h then forallb f t else false
  end.

Fixpoint existsb {X : Type} (f : X -> bool) (xs : list X) : bool :=
  match xs with
  | [] => false
  | h :: t => if f h then true else existsb f t
  end.

Definition existsb' {X : Type} (f : X -> bool) (xs : list X) : bool :=
  negb (forallb (fun x => negb (f x)) xs).

Example forallb_1 :
  forallb oddb [1;2;3] = false.
Proof. reflexivity. Qed.

Example forallb_2 :
  forallb oddb [1;3;5] = true.
Proof. reflexivity. Qed.

Example existsb_1 :
  existsb oddb [2;4;6] = false.
Proof. reflexivity. Qed.

Example existsb_2 :
  existsb oddb [2;4;5] = true.
Proof. reflexivity. Qed.

Theorem same_existsb_existsb' : forall (X : Type) (f : X -> bool) (xs : list X),
  existsb f xs = existsb' f xs.
Proof.
  intros X f xs. induction xs as [|h t].
  Case "xs = nil". unfold existsb.
    unfold existsb'. unfold forallb. reflexivity.
  Case "xs = h :: t". destruct (f h) eqn:Hfh.
    SCase "f h = true". unfold existsb. unfold existsb'.
      rewrite Hfh. simpl. rewrite Hfh. reflexivity.
    SCase "f h = false".
      unfold existsb. rewrite Hfh. fold (@existsb X).
      rewrite IHt. unfold existsb'. unfold forallb.
      rewrite Hfh. reflexivity.
Qed.
(** [] *)

(* $Date: 2013-07-17 16:19:11 -0400 (Wed, 17 Jul 2013) $ *)
