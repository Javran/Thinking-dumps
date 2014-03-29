var fs = require('fs')

var fPath = process.argv[2] 

var lines = fs.readFileSync(fPath).toString().split('\n')
    
console.log(lines.length - 1)
