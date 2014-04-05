(** * Basics: Functional Programming in Coq *)
 
(* begin hide *)
Definition admit {T: Type} : T.  Admitted.

Theorem plus_O_n : forall n : nat, 0 + n = n.
Proof.
  intros n. reflexivity.  Qed.

Fixpoint beq_nat (n m : nat) : bool :=
  match n with
  | O => match m with
         | O => true
         | S m' => false
         end
  | S n' => match m with
            | O => false
            | S m' => beq_nat n' m'
            end
  end.

Fixpoint ble_nat (n m : nat) : bool :=
  match n with
  | O => true
  | S n' =>
      match m with
      | O => false
      | S m' => ble_nat n' m'
      end
  end.

Theorem plus_1_l : forall n:nat, 1 + n = S n. 
Proof.
  intros n. reflexivity.  Qed.

Theorem mult_0_l : forall n:nat, 0 * n = 0.
Proof.
  intros n. reflexivity.  Qed.

Fixpoint plus' (n : nat) (m : nat) : nat :=
  match n with
    | O => m
    | S n' => S (plus' n' m)
  end.
(* end hide *)

(** ** Exercise answers *)

(** **** Exercise: 1 star (nandb) *)

(** This function should return [true] if either or both of
    its inputs are [false]. *)
Definition nandb (b1:bool) (b2:bool) : bool :=
  negb (andb b1 b2).

Example test_nandb1: nandb  true false = true.
Proof. reflexivity. Qed.
Example test_nandb2: nandb false false = true.
Proof. reflexivity. Qed.
Example test_nandb3: nandb false true  = true.
Proof. reflexivity. Qed.
Example test_nandb4: nandb true  true  = false.
Proof. reflexivity. Qed.

(** **** Exercise: 1 star (andb3) *)
(** This function should return [true] when all of its inputs
    are [true], and [false] otherwise. *)

Definition andb3 (b1:bool) (b2:bool) (b3:bool) : bool :=
  andb (andb b1 b2) b3.

Example test_andb31: andb3 true  true  true  = true.
Proof. reflexivity. Qed.
Example test_andb32: andb3 false true  true  = false.
Proof. reflexivity. Qed.
Example test_andb33: andb3 true  false true  = false.
Proof. reflexivity. Qed.
Example test_andb34: andb3 true  true  false = false.
Proof. reflexivity. Qed.

(** **** Exercise: 1 star (factorial) *)
(** the standard factorial function:
<<
    factorial(0)  =  1 
    factorial(n)  =  n * factorial(n-1)     (if n>0)
>> *)
Fixpoint factorial (n:nat) : nat := 
  match n with
  | O => S O
  | S n' => mult n (factorial n')
  end.

Example test_factorial1: factorial 3 = 6.
Proof. reflexivity. Qed.
Example test_factorial2: factorial 5 = mult 10 12.
Proof. reflexivity. Qed.

(** **** Exercise: 2 stars (blt_nat) *)
(** The [blt_nat] function tests [nat]ural numbers for [l]ess-[t]han,
    yielding a [b]oolean.  Instead of making up a new [Fixpoint] for
    this one, define it in terms of a previously defined function.  
*)

Definition blt_nat (n m : nat) : bool :=
  ble_nat (S n) m.

Example test_blt_nat1: blt_nat 2 2 = false.
Proof. reflexivity. Qed.
Example test_blt_nat2: blt_nat 2 4 = true.
Proof. reflexivity. Qed.
Example test_blt_nat3: blt_nat 4 2 = false.
Proof. reflexivity. Qed.

(** **** Exercise: 1 star (plus_id_exercise) *)
(** fill in the proof. *)

Theorem plus_id_exercise : forall n m o : nat,
  n = m -> m = o -> n + m = m + o.
Proof.
  intros n m o.
  intros Hnm Hmo.
  rewrite -> Hnm.
  rewrite <- Hmo.
  reflexivity.
  Qed.

(** **** Exercise: 2 stars (mult_S_1) *)
Theorem mult_S_1 : forall n m : nat,
  m = S n -> 
  m * (1 + n) = m * m.
Proof.
  intros n m.
  intros H.
  rewrite -> plus_1_l.
  rewrite <- H.
  reflexivity. Qed.

(** **** Exercise: 1 star (zero_nbeq_plus_1) *)
Theorem zero_nbeq_plus_1 : forall n : nat,
  beq_nat 0 (n + 1) = false.
Proof.
  intros n. destruct n as [|n'].
  reflexivity.
  reflexivity. Qed.

(** **** Exercise: 2 stars (boolean functions) *)
(** Use the tactics you have learned so far to prove the following 
    theorem about boolean functions. *)

Theorem identity_fn_applied_twice : 
  forall (f : bool -> bool), 
  (forall (x : bool), f x = x) ->
  forall (b : bool), f (f b) = b.
Proof.
  intros f Hfix b.
  rewrite -> Hfix.
  rewrite -> Hfix.
  reflexivity. Qed.

(** Now state and prove a theorem [negation_fn_applied_twice] similar
    to the previous one but where the second hypothesis says that the
    function [f] has the property that [f x = negb x].*)
Theorem negation_fn_applied_twice :
  forall (f : bool -> bool), 
  (forall (x : bool), f x = negb x) ->
  forall (b : bool), f (f b) = b.
Proof.
  intros f Hnot b.
  rewrite -> Hnot.
  rewrite -> Hnot.
  destruct b.
  reflexivity.
  reflexivity.
  Qed.

(** **** Exercise: 2 stars (andb_eq_orb) *)
(** Prove the following theorem.  (You may want to first prove a
    subsidiary lemma or two.) *)

Theorem andb_eq_orb : 
  forall (b c : bool),
  (andb b c = orb b c) ->
  b = c.
Proof.
  intros b c.
  destruct b.
  simpl.
  intro H.
  rewrite -> H.
  reflexivity.
  simpl.
  intro H.
  exact H.
  Qed.

(** **** Exercise: 3 stars (binary) *)
(** Consider a different, more efficient representation of natural
    numbers using a binary rather than unary system.  That is, instead
    of saying that each natural number is either zero or the successor
    of a natural number, we can say that each binary number is either

      - zero,
      - twice a binary number, or
      - one more than twice a binary number.

    (a) First, write an inductive definition of the type [bin]
        corresponding to this description of binary numbers. 

    (Hint: Recall that the definition of [nat] from class,
    Inductive nat : Type :=
      | O : nat
      | S : nat -> nat.
    says nothing about what [O] and [S] "mean."  It just says "[O] is
    in the set called [nat], and if [n] is in the set then so is [S
    n]."  The interpretation of [O] as zero and [S] as successor/plus
    one comes from the way that we _use_ [nat] values, by writing
    functions to do things with them, proving things about them, and
    so on.  Your definition of [bin] should be correspondingly simple;
    it is the functions you will write next that will give it
    mathematical meaning.)

    (b) Next, write an increment function for binary numbers, and a
        function to convert binary numbers to unary numbers.

    (c) Write some unit tests for your increment and binary-to-unary
        functions. Notice that incrementing a binary number and
        then converting it to unary should yield the same result as first
        converting it to unary and then incrementing. 
*)

Inductive bin : Type :=
  | Zero : bin
  | Twice : bin -> bin
  | Onemore : bin -> bin.

(* equality test for [nat] *)
Fixpoint unary_eq (n1 n2 : nat) : bool :=
  match n1 with
  | O => 
      match n2 with
      | O => true
      | _ => false
      end
  | S n1' =>
      match n2 with
      | O => false
      | S n2' => unary_eq n1' n2'
      end
  end.

Example unary_eq_test1 : unary_eq O O = true.
Proof. reflexivity. Qed.
Example unary_eq_test2 : unary_eq (S O) (S O) = true.
Proof. reflexivity. Qed.
Example unary_eq_test3 : unary_eq (S (S O)) (S O) = false.
Proof. reflexivity. Qed.
Example unary_eq_test4 : unary_eq (S O) (S (S O)) = false.
Proof. reflexivity. Qed.

(* convert [bin] to [nat] *)
Fixpoint bin_to_unary (n : bin) : nat :=
  match n with
  | Zero => O
  | Twice n' => bin_to_unary(n') + bin_to_unary(n')
  | Onemore n' => S (bin_to_unary n')
  end.

Example bin_to_unary_test1 : bin_to_unary Zero = 0.
Proof. reflexivity. Qed.
Example bin_to_unary_test2 : bin_to_unary (Twice (Onemore Zero)) = 2.
Proof. reflexivity. Qed.
Example bin_to_unary_test3 : bin_to_unary (Onemore (Twice (Onemore Zero))) = 3.
Proof. reflexivity. Qed.

(* equality test for [bin] *)
Fixpoint bin_eq (b1 b2 : bin) : bool :=
  unary_eq (bin_to_unary b1)
           (bin_to_unary b2).

Example bin_eq_test1 : bin_eq Zero Zero = true.
Proof. reflexivity. Qed.
Example bin_eq_test2 : bin_eq (Twice Zero) Zero = true.
Proof. reflexivity. Qed.
Example bin_eq_test3 : bin_eq Zero (Onemore Zero) = false.
Proof. reflexivity. Qed.

(* successor for [bin] *)
Fixpoint bin_succ (n : bin) : bin :=
  match n with
  (* 0 + 1 = 1 *)
  | Zero => Onemore Zero
  (* 2*x + 1 = (2*x) + 1 *)
  | Twice n' => Onemore n'
  (* (2*x + 1) + 1 = 2 *(x + 1) *)
  | Onemore n' => Twice (bin_succ n')
  end.

Example bin_succ_test1 :
  bin_eq (bin_succ (Twice (Twice Zero)))
         (Onemore Zero)
         = true.
Proof. reflexivity. Qed.
Example bin_succ_test2 :
  bin_eq (bin_succ (Twice (Onemore Zero)))
         (Onemore (Onemore Zero))
         = true.
Proof. reflexivity. Qed.
Example bin_succ_test3 :
  bin_eq (bin_succ (Onemore (Twice (Onemore Zero)))) 
         (Twice (Onemore (Onemore Zero)))
         = true.
Proof. simpl. reflexivity. Qed.

(** **** Exercise: 2 stars, optional (decreasing) *)
(** write a sensible [Fixpoint] definition that
    _does_ terminate on all inputs, but that Coq will _not_ accept
    because of this restriction. *)

(** the trick is to make n increase for some cases while
   we make sure the defintion does terminate.
[[
Fixpoint a_loop (n : nat) : nat :=
  match n with
    | O => a_loop (S n)
    | _ => O
  end.
]]
*)