var http = require('http')
var concat = require('concat-stream')

var urls = process.argv.slice(2)
var datas = [false, false, false]

function checkAndPrint()
{
    if (datas.every( function (x) {return x} )) {
        datas.forEach(
            function (data) { console.log(data) })
    }
}

urls.forEach(
    function (url,ind) {
        http.get(
            url,
            function (res) {
                res.pipe(
                    concat(
                        function (data) {
                            datas[ind] = data.toString()
                            checkAndPrint()
                        }))
            })
    })
