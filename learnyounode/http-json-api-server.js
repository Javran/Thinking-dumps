var httpM = require('http')
var fsM = require('fs')
var urlM = require('url')

var port = process.argv[2]

function queryToDate (q) {
    return new Date(q['iso'])
}

var callbacks = {}

callbacks['/api/parsetime'] =
    function (d) {
        return { hour   : d.getHours()
               , minute : d.getMinutes()
               , second : d.getSeconds()
               }
    }
callbacks['/api/unixtime'] =
    function (d) {
        return { unixtime : d.getTime() }
    }

function queryToResult (q, pathname) {
    var callback = callbacks[pathname]
    var obj = "Error"
    if ( callback ) {
        obj = callback( queryToDate( q ) )
    }
    return JSON.stringify(obj)
}

httpM.createServer(
    function (req, res) {
        var data = urlM.parse(req.url, true)

        res.writeHead(200, { 'Content-Type': 'application/json' })

        var str = queryToResult( data['query'], data['pathname'] )
        res.end(str)
    })
    .listen(port)
