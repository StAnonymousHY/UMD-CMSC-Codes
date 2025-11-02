
const http = require('http');
const portNumber = 5001;
const httpSuccessStatus = 200;
const webServer = http.createServer((request, response) => {
	response.writeHead(httpSuccessStatus, {'Content-type':'text/html'});
	response.write(`<h1>Web Server (NodeJS based) Running (Time ${new Date()})</h1>`);
	response.end();
});

webServer.listen(portNumber); 
console.log(`Web server is running at http://localhost:${portNumber}`);
console.log("Type stop to shutdown the server");
process.stdin.setEncoding("utf8"); /* encoding */
process.stdin.on('readable', () => {  /* on equivalent to addEventListener */
	const dataInput = process.stdin.read();
	if (dataInput !== null) {
		const command = dataInput.trim();
		if (command === "stop") {
			// Not using console.log as it adds a newline by default
			process.stdout.write("Shutting down the server"); 
            process.exit(0);  /* exiting */
        } else {
			/* After invalid command, we cannot type anything else */
			process.stdout.write(`Invalid command: ${command}`);
		}
		process.stdin.resume(); // Allows the code to process next request
    }
});
