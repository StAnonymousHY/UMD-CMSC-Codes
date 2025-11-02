const http = require('http');
const fs = require('fs');
const portNumber = 5000;
const httpResponseStatusCodeOK = 200;
const httpResponseStatusCodeNotFound = 404;
const webServer = http.createServer((request, response) => {
	const url = require('url');
	const name = url.parse(request.url, true).query.imageName;
	
	if (name === 'umcp') {
		const fileName = 'images/umcp.jpg';
		fs.stat(fileName, (err, fileInfo) => {
			if (err) {
				console.error(err);
				response.writeHead(httpResponseStatusCodeNotFound, {'Content-type': 'text/html'});
				response.write('<h1>Image file not found</h1>');
				response.end();
            } else {
				const image = fs.readFileSync(fileName);
				response.contentType = "image/jpg";
				response.contentLength = fileInfo.size;
				response.end(image, "binary");
			}
		});
    } else {
		response.writeHead(httpResponseStatusCodeNotFound, {'Content-type': 'text/html'});
		response.write('<h1>Invalid image name specified in the url</h1>');
		response.end();
	}
});

webServer.listen(portNumber); 

console.log(`Image Web server is running at http://localhost:${portNumber}`);