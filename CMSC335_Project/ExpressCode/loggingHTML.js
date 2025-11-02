const express = require("express");   /* Accessing express module */
const app = express();  /* app is a request handler function */
const portNumber = 7000;
const morganLogger = require("morgan");

app.use(morganLogger("dev")); /* You can try dev instead of short */

app.use((request, response) => {
   response.send("<h1>We have logged the request</h1>");
});

app.listen(portNumber);
console.log(`To access server: http://localhost:${portNumber}`);
