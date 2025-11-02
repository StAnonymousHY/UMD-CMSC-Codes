const express = require("express");   /* Accessing express module */
const app = express();  /* app is a request handler function */
const portNumber = 8001;

app.get("/", (req, res) => {
   res.send("get request detected for CMSC335");
});

app.post("/", (req, res) => {
   res.send("post request detected for CMSC335")
});

app.put("/", (req, res) => {
   res.send("put request detected for CMSC335")
});

app.delete("/", (req, res) => {
   res.send("delete request detected for CMSC335")
});

app.listen(portNumber);
console.log(`To access server: http://localhost:${portNumber}`);
