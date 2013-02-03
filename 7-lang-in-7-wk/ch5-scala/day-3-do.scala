#!/bin/sh
       TAGSOUP_LIB=/usr/share/tagsoup/lib/tagsoup.jar
       exec /usr/bin/env scala -classpath ${TAGSOUP_LIB} "$0" "$@"
!#

/*
	IMPORTANT: to get this source code work, you might need tagsoup:
	    http://mercury.ccil.org/~cowan/XML/tagsoup/

	After getting tagsoup ready, please modify TAGSOUP_LIB accordingly
*/

// Task #1: count the number of links
// Task #2: make sizer follow links and compute size for all pages it links to

import scala.io._
import scala.actors._
import scala.xml.parsing.NoBindingFactoryAdapter;
import Actor._

import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl;
import org.xml.sax.InputSource;
import java.io.ByteArrayInputStream;

// judge if we should follow the URL or not
def shouldFollowURL(url:String) = {
	( ! url.contains( '#' ) ) && /* no tags */
	( url != "/" ) && /* no self-link */
	( url.startsWith( "/" ) ) /* relative links only */
}

class PageInformation(
	val url: String,
	val linkCount: Int,
	val followedLinkCount: Int,
	val pageSize: Int,
	val followedLinkSize: Int,
	val error: Boolean) {

	def print() = {
		println( "URL: " + url )

		if (error) {
			println( "<No information avaliable>" )
		} else {

			println( "Link count: " + linkCount )
			println( "Page size: " + pageSize )
			println( "Link count (followed): " + followedLinkCount )
			println( "Page size (followed): " + followedLinkSize )
			println( "Total downloaded size: " + (pageSize + followedLinkSize) )
		}

		println()
	}

}

object PageLoader {
	def getPageInformation(url: String): PageInformation = {
		try {
			val content = Source
				.fromURL(url)
				.mkString

			// parse HTML page using tagsoup
			val adapter = new NoBindingFactoryAdapter()

			val data = adapter.loadXML(
				/* source */
				new InputSource(new ByteArrayInputStream(content.getBytes())),
				/* parser */
				new SAXFactoryImpl().newSAXParser())

			val links = ( data \\ "a" \\ "@href" ).map( l => l.toString() ) 

			val followedLinkSize = links
				.filter( u => shouldFollowURL( u ) ) /* filter links first */
				.map( u => { /* download all links */
					try {
						Source.fromURL( url + u ).mkString.length
					} catch {
						case _ => -1
					}	
				})
				.filter( x => x >= 0 )

			val subSizeCount = followedLinkSize.foldLeft( 0 )( (acc, i) => acc + i )

			new PageInformation(
				url, 
				links.length, 			// linkCount
				followedLinkSize.length,  	// followedLinkCount
				content.length, 		// pageSize
				subSizeCount, 			// followedLinkSize
				false)

		} catch {
			case _ => new PageInformation(url, 0, 0, 0, 0, true)
		}
	}
}

val urls = List(
	"http://www.google.com/",
	"http://www.amazon.com/",
	"http://www.gnu.org/",
	"http://www.bing.com/")

val caller = self

urls.foreach( u => {
	actor { caller ! PageLoader.getPageInformation(u) }
} )

(0 until urls.size).foreach( _ => {
	receive {
		case pageInfo:PageInformation => pageInfo.print()
	}
})
