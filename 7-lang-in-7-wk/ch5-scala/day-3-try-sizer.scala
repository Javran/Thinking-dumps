#!/usr/bin/env scala
!#

import scala.io._
import scala.actors._
import Actor._

object PageLoader {
	def getPageSize(url: String) = {
		try {
			Source
				.fromURL(url)
				.mkString
				.length
		} catch {
			case _ => {
				println( "Error occurred." )
				-1
			}
		}
	}
}

val urls = List(
	"http://www.example.com/",
	"http://www.amazon.com/",
	"http://www.google.com/",
	"http://www.cnn.com",
	"http://www.scala-lang.org/")

def timeMethod(method: () => Unit) = {
	val startTime = System.nanoTime
	method()
	val endTime = System.nanoTime
	println( "Method took " + (endTime-startTime)/1e9 + " seconds.")
}

timeMethod( () => timeMethod( () => println("test timeMethod itself, just for fun...")) )

def getPageSizeSequentially() = {
	urls.foreach( u => {
		println( "Url: \"%s\" fetched, size: %d".format(u, PageLoader.getPageSize(u)) )
	})
}

getPageSizeSequentially
