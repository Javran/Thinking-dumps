var m = require('./make-it-modular-m')

var filePath = process.argv[2]
var ext  = process.argv[3] 

m.listAndFilter(filePath, ext,
        function(err,data) {
            data.forEach(
                function(x,ind,arr)
                {
                    console.log(x)
                })
        })
