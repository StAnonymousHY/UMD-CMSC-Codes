const express = require("express");
const app = express();
const session = require("express-session"); /* For sessions */
const cookieParser = require("cookie-parser");
const bodyParser = require("body-parser");
const portNumber = 3000;

app.use(bodyParser.urlencoded({ extended: false }));
app.use(cookieParser());
app.use(
  session({
    resave: true,
    saveUninitialized: false,
    secret: "putsomethingsecretheredontshow", // use .env for secret string
  })
);

app.post("/login", (request, response) => {
  let message;

  /* 
   * Do not put passwords like we are doing; this is just to practice sessions. 
   * We are allowing two users: peter (password terps) and mary (password testudo) 
   * This will allow us to simultaneously have two clients (use insomnia for one 
   * Postman for the other) 
   */

  if ( (request.body.user === "peter" && request.body.password === "terps") || 
       (request.body.user === "mary" && request.body.password === "testudo") ) {
    request.session.user = request.body.user;
    request.session.cart = "";
    request.session.save();
    message = `User ${request.session.user} has logged in`;
  } else {
    message = "Invalid user";
  }
  response.send(message);
});

app.get("/browse", (request, response) => {
  let message;
  if (request.session.user != undefined) {
    message = `Welcome back ${request.session.user}, browse`;
  } else {
    message = "You have not logged in";
  }
  response.send(message);
});

app.post("/buy", (request, response) => {
  let message;

  if (request.session.user != undefined) {
    request.session.cart += request.body.item + " ";
    message = `${request.body.item} added to your cart`;
  } else {
    message = "You have not logged in";
  }
  response.send(message);
});

app.post("/checkout", (request, response) => {
  let message;

  if (request.session.user != undefined) {
    message = `Items you are buying are ${request.session.cart}`;
  } else {
    message = "You have not logged in";
  }
  response.send(message);
});

app.post("/logout", (request, response) => {
  let message;

  if (request.session.user != undefined) {
    request.session.destroy();
    message = "You have logged out";
  } else {
    message = "You were not logged in";
  }
  response.send(message);
});

console.log(`Server listening on port ${portNumber}`);
const homeURL = `http://localhost:${portNumber}`;
console.log(homeURL);
app.listen(portNumber);
