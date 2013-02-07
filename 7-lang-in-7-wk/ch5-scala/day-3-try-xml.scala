#!/usr/bin/env scala
!#

val movies =
	<movies>
		<!--
			we can use xml-style comment.
		-->
		<movie genre="action">Pirates of the Caribbean</movie>
		<movie genre="fairytale">Edward Scissorhands</movie>
		<movie>The Shawshank Redemption</movie> <!-- test what if genre is not set -->
		<book>Seven Languages in Seven Weeks</book>
	</movies>


// TODO: learn XPath
// the related link can be found at:
//     

println( "XML Content:" )
println( movies.text )

val movieNodes = movies \ "movie"

movieNodes.foreach( m => {
	println( "Movie: " + m )
	val g = m \ "@genre"
	if (g.isEmpty)
		println( ">>> No genre found" )
	else
		println( ">>> Genre: " + g )
})
