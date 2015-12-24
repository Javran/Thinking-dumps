IN: day-3.tuples

USING: kernel accessors ;

TUPLE: cart-item name price quantity ;

: <dollar-cart-item> ( name -- cart-item )
    1.00 1 cart-item boa ;

: <one-cart-item> ( -- cart-item )
    T{ cart-item { quantity 1 } } ;


TUPLE: checkout item-count base-price taxes shipping total-price ;

USING: math sequences ;

! calculate sum of a sequences of things
: sum ( seq -- n )
    0 [ + ] reduce ;

! get total number of items from a sequence of cart-items
: cart-item-count ( cart -- count )
    [ quantity>> ] map sum ;

! get item price for a specific cart-item
: cart-item-price ( cart-item -- price )
    [ price>> ] [ quantity>> ] bi * ;

! get total price from a sequence of cart-items
: cart-base-price ( cart -- price )
    [ cart-item-price ] map sum ;

: <base-checkout> ( item-count base-price -- checkout )
    ! item-count and base-price is on top of the stack already
    f f f
    checkout boa ;

: <checkout> ( cart -- checkout )
    [ cart-item-count ] [ cart-base-price ] bi
    <base-checkout> ;
