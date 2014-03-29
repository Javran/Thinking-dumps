var http = require('http')
var fs = require('fs')
var tmap = require('through2-map')

var port = process.argv[2]

http.createServer(
    function (req, res) {
        req.pipe(tmap(
            function (data) {
                return data.toString().toUpperCase()
            }))
            .pipe(res)
    })
    .listen(port)
