const express = require("express");   /* Accessing express module */
const app = express();  /* app is a request handler function */
const portNumber = 6500;

app.get("/", (request, response) => {
   response.send(`Welcome to the cmsc335 home page`);
});

app.get("/syllabus", (request, response) => {
   response.send(`Class Syllabus`);
});

app.get("/class/:semester/:test1", (request, response) => {
   response.send(`Information for semester: ${request.params.semester} ${request.params.test1}`);   
});

app.listen(portNumber);
console.log(`Try: http://localhost:${portNumber}`);
console.log(`Try: http://localhost:${portNumber}/syllabus`);
console.log(`Try: http://localhost:${portNumber}/class/summer/randomWord`);
console.log(`Try: http://localhost:${portNumber}/invalid`);