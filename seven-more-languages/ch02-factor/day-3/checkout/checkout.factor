IN: day-3.checkout

USING: kernel accessors math sequences ;

USE: day-3.tuples

! basically the book gives us an idea about keeping fields that would be filled in future:
! it keep track of what we have for now, and use placeholders (e.g. f here) to indicate
! that value is unknown up to now.
! later, as the program progresses, we put information into use and make rest of the fields
! available.
! this approach could sometimes get in the way because there is no consistent way to test if
! a field is properly filled with a value / or the value is up-to-date (given that it's possible
! the source of the information changes but the update is not reflected in that field)

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

CONSTANT: base-shipping 1.49
CONSTANT: per-item-shipping 1.00

: per-item ( checkout -- shipping )
    per-item-shipping * + base-shipping + ;

: shipping ( checkout shipping-calc -- shipping )
    ! get item count from checkout
    [ dup item-count>> ] dip
    ! calculate shipping cost and write to the checkout instance
    call >>shipping ; inline

: total ( checkout -- total-price )
    dup
    ! the total price comes from 3 sources, we add them together
    ! to yield the final result
    [ base-price>> ] [ taxes>> ] [ shipping>> ] tri + +
    >>total-price ;

: sample-checkout ( checkout -- checkout )
    [ gst-pst ] taxes
    [ per-item ] shipping
    total ;
