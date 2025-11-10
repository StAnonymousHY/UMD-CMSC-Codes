const express = require("express");
const app = express();
const cookieParser = require("cookie-parser"); /* Needed for cookie access */
const portNumber = 3000;

app.use(cookieParser()); /* Adding cookie middleware */

app.get("/", (request, response) => {
  /* Notice: using response object to set the cookie */
  response.cookie("campusLocation", "College Park", { httpOnly: true });
  response.send("We set a cookie named campusLocation with a value of College Park");
});

app.get("/setMascotCookie", (request, response) => {
  response.cookie("mascot", "testudo", { httpOnly: true });
  response.send("We set a cookie named mascot that has the value testudo");
});

app.get("/check", (request, response) => {
   /* Notice: using request object to retrieve the cookie */
   console.log(request.cookies.mascot);
   response.send(`Value of mascot cookie: ${request.cookies.mascot}`);
 });

console.log(`Server listening on port ${portNumber}`);
const homeURL = `http://localhost:${portNumber}`;
console.log(homeURL);
console.log(`${homeURL}/setMascotCookie`);
console.log(`${homeURL}/check`);

app.listen(portNumber);
