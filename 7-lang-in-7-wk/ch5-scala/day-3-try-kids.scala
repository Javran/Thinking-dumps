#!/usr/bin/env scala
!#

import scala.actors._
import scala.actors.Actor._

// these objects are used as messages
case object Poke
case object Feed
case object Done

class Kid() extends Actor {
	def act() {
		loop {
			react {
				case Poke => {
					println( self + ": <Poke>" )
					println( self + ": Ow..." )
					println( self + ": Quit it..." )
				}
				case Feed => {
					println( self + ": <Feed>" )
					println( self + ": Gurgle..." )
					println( self + ": Burp..." )
				}
				case Done => {
					println( self + ": <Done>" )
					println( self + ": Bye..." )
					exit()
				}
			}
		}
	}
}

val bart = new Kid().start
val lisa = new Kid().start

println( "Ready to poke and feed..." )
bart ! Poke
lisa ! Poke
bart ! Feed
lisa ! Feed

// stop the actors so this program will terminate
bart ! Done
lisa ! Done
