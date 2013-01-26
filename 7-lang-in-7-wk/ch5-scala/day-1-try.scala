#!/usr/bin/env scala "$0" "$@"
!#

// if you encounter some problem like:
//     "Unable to establish connection to compilation daemon"
// please make sure to set your ip-hostname map properly in /etc/hosts
// related link:
//     http://stackoverflow.com/questions/7714696/scala-hello-world-script-does-not-work
println("nice boat!")
// nice boat!

println(1+1)
// 2

println((1).+(1))
// 2

println(5 + 4 * 3)
// 17

println(5.+(4.*(3)))
// 17.0

println((5).+(4.*(3)))
// 17
