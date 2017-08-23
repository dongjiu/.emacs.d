var fs = require('fs');
fs.readFile(process.argv[2], 'utf8', function (err, data) {
	console.log(JSON.stringify(JSON.parse(data), null, 4));
});
