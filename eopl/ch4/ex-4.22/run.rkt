#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "./top.rkt")
(require "./data-structures.rkt")
(require "./printer.rkt")


(test-all)

(print-to-screen #t)

(out "Example 1")
(run "
  var x,y;
  { x = 3;
    y = 4;
    print +(x,y)
  }")

(out "Example 2")
(run "
  var x,y,z;
  { x = 3;
    y = 4;
    z = 0;
    while not(zero?(x))
    {
      z = +(z,y) ;
      x = -(x,1)
    };
    print z
  }
  ")

(out "Example 3")
(run "
  var x;
  { x = 3;
    print x;
    var x; {x = 4; print x};
    print x
  }
  ")

; TODO: need multiarg extension
(out "Example 4")
(run "
  var f,x;
  { f = proc(x) proc(y) *(x,y);
    x = 3;
    print ((f 4) x)
  }
  ")
