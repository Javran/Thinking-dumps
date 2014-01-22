var http = require('http')
var concat = require('concat-stream')

var url = process.argv[2]
var dataConcated = concat(
    function (data) { 
        text = data.toString()
        console.log(text.length)
        console.log(data.toString())
    })

http.get(
    url,
    function (response) {
        response.pipe(dataConcated)
    })
