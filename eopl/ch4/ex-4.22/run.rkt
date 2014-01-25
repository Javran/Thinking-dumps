#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "./top.rkt")
(require "./data-structures.rkt")

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
