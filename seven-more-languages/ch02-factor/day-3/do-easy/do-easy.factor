USING: kernel accessors ;
IN: day-3.do-easy

TUPLE: cart-item name price quantity ;

! ex 1: create cart-item with default name and quantity
: <cart-item-with-price> ( p -- obj )
    "default-cart-item" swap
    ! now we have: name, price
    1 cart-item boa ;

USE: math

: apply-discount ( obj percent -- obj )
    swap
    ! object on top
    [ 
        ! after "change-price" consumes two elements from the stack
        ! all we have is the percentage, and on top of it the actually
        ! price is pushed, all we need to do is modify it -- by simply
        ! multiply the percentage
        *
    ]
    change-price ;
