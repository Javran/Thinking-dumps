#!/usr/bin/env scala
!#

class Person(first_name: String) {
	println("Outer constructor")
	
	def this(first_name: String, last_name: String) {
		this(first_name)
		println("Inner constructor")
	}

	def talk() = println( "Hi" )
}

println("Create Bob:")
val bob = new Person("Bob")

println("Create Bob Tate:")
val botTate = new Person("Bob", "Tate")
