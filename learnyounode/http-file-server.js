var http = require('http')
var fs = require('fs')

var port = process.argv[2]
var filePath = process.argv[3]

http.createServer(
    function (req, res) {
        fs.createReadStream(filePath).pipe(res)
    })
    .listen(port)
