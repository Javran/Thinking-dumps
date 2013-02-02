#!/usr/bin/env scala
!#

import scala.io._
import scala.actors._
import Actor._

import scala.xml.XML

object PageLoader {
	def getPageSize(url: String) = {
		try {
			val content = Source
				.fromURL(url)
				.mkString

			// TODO: find the right way of parsing HTML
			val parsed = XML.loadString(content)
			
			content.length
		} catch {
			case e:Exception => {
				println( "Error occurred:" + e.getMessage() )
				-1
			}
		}
	}
}

val urls = List(
	"http://www.example.com/",
	"http://www.w3schools.com/",
	"http://www.gnu.org/")

def getPageSizeConcurrently() = {
	val caller = self

	urls.foreach( u => {
		actor { caller ! (u, PageLoader.getPageSize(u)) }
	} )

	(0 until urls.size).foreach( _ => {
		receive {
			case (url, size) =>
			{
				println( "Url: \"%s\"\nSize: %d".format(url, size) )
			}
		}
	})
}

getPageSizeConcurrently
