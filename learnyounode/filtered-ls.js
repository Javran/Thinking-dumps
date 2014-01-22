var fs = require('fs')
var path = require ('path')

var filePath = process.argv[2]
var ext  = process.argv[3] 

var files =
    fs.readdir(
        filePath,
        function (err, list)
        {
            if (err)
                throw err
            list.filter(
                function (f)
                {
                    return path.extname(f) == '.' + ext
                }).forEach(function(x,ind,arr) {
                    console.log(x)
                })
            
        })

