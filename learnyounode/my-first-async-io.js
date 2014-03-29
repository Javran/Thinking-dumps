var fs = require('fs')
var fPath = process.argv[2] 

function callback(err, data)
{
    if (err)
        throw err;

    var lines = data.toString().split('\n')

    console.log(lines.length - 1);
}

fs.readFile(fPath, callback);
