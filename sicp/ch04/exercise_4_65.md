By definition, a person is a "wheel" in an organization
if he supervisors someone who is in turn a supervisor.
Therefore how many times a person can appear in the query result
partially depends on how many supervisors that he/she supervisors.

From the database we have following relationships:

* Oliver Warbucks supervises Eben Scrooge
    * Eben Scrooge supervises Robert Cratchet
* Ben Bitdiddle supervises Alyssa P. Hacker
    * Alyssa P. Hacker supervises Louis Reasoner
* Oliver Warbucks supervises Ben Bitdiddle
    * Ben Bitdiddle supervises Alyssa P. Hacker
    * Ben Bitdiddle supervises Cy D. Fect
    * Ben Bitdiddle supervises Lem E. Tweakit

And I think this is where the 5 query results come from.
