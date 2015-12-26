IN: day-3.do-medium.tests

USE: day-3.do-medium
USE: tools.test

{ t } [ 1000 new-tax-calc  
        100 is-close-enough ] unit-test

{ t } [ 6000 new-tax-calc  
        720 is-close-enough ] unit-test
