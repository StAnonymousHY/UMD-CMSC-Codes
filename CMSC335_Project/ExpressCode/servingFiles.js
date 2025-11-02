const path = require("path");
const express = require("express");   /* Accessing express module */
const app = express();  /* app is a request handler function */
const portNumber = 7001;

console.log(`\n__dirname: ${__dirname}`);
/* Notice serverStaticFiles is not part of the url to find files */
const publicPath = path.resolve(__dirname, "serverStaticFiles");
console.log(`publicPath: ${publicPath}`);

/* Adding the middleware */
app.use(express.static(publicPath));

app.listen(portNumber);
console.log("============================");
let message = `To access files under ${publicPath} type \nname of the file`;
message += ` (e.g., armory.jpg) after http://localhost:${portNumber}/`;
console.log(message);

