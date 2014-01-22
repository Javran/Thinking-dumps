var fs = require('fs')
var path = require ('path')

var filePath = process.argv[2]
var ext  = process.argv[3] 

function listAndFilter(filePath, ext, callback)
{
    fs.readdir(
        filePath,
        function (err, list)
        {
            if (err)
                return callback(err)
            list = list.filter(
                function (f)
                {
                    return path.extname(f) == '.' + ext
                })
           callback(null, list) 
        })
}

module.exports.listAndFilter = listAndFilter
