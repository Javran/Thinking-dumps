#!/usr/bin/env scala
!#

class Person(val name: String) {
	def talk(message: String) = println(name + " says \"" + message + "\"")
	def id(): String = name
}

class Employee(
		override val name: String,
		val number: Int) extends Person(name) {

	override def talk(message: String) {
		println(name + " with number " + number
			+ " says \"" + message + "\"")
	}

	override def id(): String = number.toString
}

val talkStr = "Extend or extend not, There is no try."

val employee = new Employee("Yoda", 4)
employee.talk(talkStr)

val person = new Person("Yoda")
person.talk(talkStr)

// I'll test 'trait' here
trait Nice {
	def greet() = println( "Howdily doodily." )
}

class Character(override val name: String) 
		extends Person(name)
		with Nice
	
val flanders = new Character("Ned")
flanders.greet
// Howdily doodily
