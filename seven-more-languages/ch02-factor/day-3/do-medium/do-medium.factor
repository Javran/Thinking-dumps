USING: kernel math ;
IN: day-3.do-medium

! we will have fun by making the tax-calculation
! a little more complicated:
! if the price is less than 4000, we apply a tax rate of 0.1 to it
! if the price exceeds 4000, we only apply a tax rate of 0.1 to 4000
! and apply 0.16 to the rest of the price

! everything is prefixed with "nt" for "new tax calculation"

CONSTANT: nt-rate-low 0.1
CONSTANT: nt-rate-high 0.16

CONSTANT: nt-threshold 4000

: new-tax-calc ( price -- taxes )
    dup
    nt-threshold >
    [ nt-threshold - 
      nt-rate-high *
      4000 nt-rate-low * +
    ]
    [ nt-rate-low * ] 
    if ;

CONSTANT: epsilon 1e-6

! test if two numbers are close enough
! (in other words, less than a value called epsilon here)
: is-close-enough ( x y -- b )
    - abs
    epsilon <= ; inline


USE: day-3.checkout
! let's try this shipping scheme:
! if total price >= 200, we get free shipping
! if total price >= 40
!   if item count <= 10, still free shipping
!   otherwise, apply a 20% off on shipping (* 0.8)
! if none of the below applies, apply normal shipping price
! note that there is a small challenge here: unlike "per-item" in the book
! we now have to know the total price thus need to prepare 2 pieces of info on
! the stack.
: new-shipping-calc ( item-count total-price -- shipping )
    dup 200 >=
    ! free shipping
    [ 2drop 0 ]
    [ 
        dup 40 >=
        [
            over 10 <=
            ! free shipping
            [ 2drop 0 ]
            ! 20% off
            [ drop per-item
              0.8 *
            ]
            if
        ]
        ! apply old strategy
        [ drop per-item ]
    ]
    if ;
