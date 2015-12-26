IN: day-3.do-medium.tests

USE: day-3.do-medium
USE: day-3.checkout
USE: math
USE: tools.test
USE: kernel
USE: accessors

{ t } [ 1000 new-tax-calc  
        100 is-close-enough ] unit-test

{ t } [ 6000 new-tax-calc  
        720 is-close-enough ] unit-test

! free shipping, case 1
{ t } [ 20 400 new-shipping-calc
        0 is-close-enough ] unit-test
{ t } [ 10 45 new-shipping-calc
        0 is-close-enough ] unit-test
! get shipping 20% off
{ t } [ 20 45 new-shipping-calc
        20 per-item 0.8 *
        is-close-enough ] unit-test
! nothing special
{ t } [ 123 10 new-shipping-calc
        123 per-item
        is-close-enough ] unit-test

! test the whole process of calculation shipping cost
{ t }
[ 123 20 f f f checkout boa
  [ new-shipping-calc ]
  new-shipping
  shipping>>
  123 20 new-shipping-calc
  is-close-enough ] unit-test
