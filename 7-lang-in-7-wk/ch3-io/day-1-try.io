#!/bin/env io
"* say hello to everyone:\n" print
"nice boat!\n" print

"* clone object:\n" print
Vehicle := Object clone
Vehicle print

"* use slots:\n" print
# can only use ':=' because this slot has not yet been created
Vehicle description := "Init description"
Vehicle description print
"\n" print

"* change slot content:\n" print
# both ':=' and '=' operators will do
Vehicle description = "Something to take you places"
Vehicle description print
"\n" print

"* print slot names:\n" print
Vehicle slotNames print
"\n" print

"* type of Vehicle:\n" print
Vehicle type print
"\n" print

"* type of Object:\n" print
Object type print
"\n" print
