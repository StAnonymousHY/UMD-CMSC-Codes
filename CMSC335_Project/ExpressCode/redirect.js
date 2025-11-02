const path = require("path");
const express = require("express");   /* Accessing express module */
const app = express();  /* app is a request handler function */
const portNumber = 7002;

app.use(express.static(path.resolve(__dirname, "serverStaticFiles")));

/* If file not found in serverStaticFiles we will redirect to www.cs.umd.edu */
app.use((request, response) => {
   response.redirect("http://www.cs.umd.edu/");
});

app.listen(portNumber);
console.log(`\nFor file access try: http://localhost:${portNumber}/Testudo.jpg`)
console.log(`For redirect example try: http://localhost:${portNumber}/notValid.jpg`);
console.log(`For redirect example try: http://localhost:${portNumber}`);