USING: kernel accessors ;
IN: day-3.do-easy.tests

USE: day-3.do-easy
USE: tools.test
USE: math

{ 1234 } [ 1234 <cart-item-with-price> price>> ] unit-test
{ t } [ 1000 <cart-item-with-price>
        0.9 apply-discount
        price>>
        900 number= ] unit-test
