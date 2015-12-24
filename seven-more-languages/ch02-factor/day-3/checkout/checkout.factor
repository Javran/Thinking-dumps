IN: day-3.checkout

USING: kernel accessors math sequences ;

USE: day-3.tuples

TUPLE: checkout item-count base-price taxes shipping total-price ;

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

CONSTANT: gst-rate 0.05
CONSTANT: pst-rate 0.09975

: gst-pst ( price -- taxes )
    [ gst-rate * ] [ pst-rate * ] bi + ;

: taxes ( checkout taxes-calc -- taxes )
    [ dup base-price>> ] dip
    ! now the structure looks like: checkout base-price taxes-calc
    call
    ! store the result in checkout instance
    >>taxes
    ; inline
