const http = require('http');

const portNumber = 4000;
const httpSuccessStatus = 200;
const webServer = http.createServer((request, response) => {
	response.writeHead(httpSuccessStatus, {'Content-type':'text/html'});
	response.write(`<h1>Web Server (NodeJS based) Running (Time ${new Date()})</h1>`);
	response.end(); 
});

webServer.listen(portNumber); 

console.log(`Web server is running at http://localhost:${portNumber}`);