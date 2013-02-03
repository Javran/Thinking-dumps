#!/bin/sh
       TAGSOUP_LIB=/usr/share/tagsoup/lib/tagsoup.jar
       exec /usr/bin/env scala -classpath ${TAGSOUP_LIB} "$0" "$@"
!#

/*
	IMPORTANT: to get this source code work, you might need tagsoup:
	    http://mercury.ccil.org/~cowan/XML/tagsoup/

	After getting tagsoup ready, please modify TAGSOUP_LIB accordingly
*/

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

			val parserFactory = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl()
			val parser = parserFactory.newSAXParser()
			val source = new org.xml.sax.InputSource(new java.io.ByteArrayInputStream(content.getBytes()))
			val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
			val data = adapter.loadXML(source, parser)

			println( "links: %d".format( (data \\ "a" \\ "@href").length ) )

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
