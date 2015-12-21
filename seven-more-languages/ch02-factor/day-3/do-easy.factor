IN: day-3.do-easy

USE: kernel

TUPLE: cart-item name price quantity ;

! ex 1: create cart-item with default name and quantity
: <cart-item-with-price> ( p -- obj )
    "default-cart-item" swap
    ! now we have: name, price
    1 cart-item boa ;


! "change-price" not found???
! USE: math

! : apply-discount ( obj percent -- obj )
!    swap
!    [ * ]
!    change-price ;

! USE: prettyprint

! 123 <cart-item-with-price>

! dup .
