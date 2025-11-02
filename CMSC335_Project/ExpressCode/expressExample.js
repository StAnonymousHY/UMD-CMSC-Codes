const express = require("express"); /* Accessing express module */
const portNumber = 5000;

const app = express(); /* app is a request handler function */

app.use((request, response) => {
  console.log(`Received url: ${request.url}`);
  response.send(`Request received by server (port ${portNumber}). Check node console`);
});


/* In future examples, for simplicity, we will use app.listen(portNumber) */
app.listen(portNumber, (err) => {
  if (err) {
    console.log("Starting server failed.");
  } else {
    console.log(`To access server: http://localhost:${portNumber}`);
  }
});
