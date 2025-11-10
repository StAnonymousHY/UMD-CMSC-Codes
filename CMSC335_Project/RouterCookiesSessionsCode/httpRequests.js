const express = require("express");
const app = express();
const path = require("path");
const portNumber = 7003;
const bodyParser = require("body-parser");

app.use(bodyParser.urlencoded({ extended: false }));

app.set("view engine", "ejs");
app.set("views", path.resolve(__dirname, "views"));

/* This endpoint and the next use the same URI */
app.get("/", (req, res) => {
  res.send("<h1>Terp (GET) request received</h1>");
});

app.post("/", (req, res) => {
  res.send("<h1>Terp (POST) request received</h1>");
});

/* "http://localhost:7003/getEmpInfo?age=35&salary=888" */
app.get("/getEmpInfo", (req, res) => {
   res.send(`Received age: ${req.query.age} salary: ${req.query.salary}`);
});

app.get("/getEmpInfoJSON", (req, res) => {
  const student = { name: "Laura", age: 35 };
  res.json(student);
});

app.get("/getLetter", (req, res) => {
   const variables = { semester: "Summer" };
   res.render("company", variables);
});

/* 
http://localhost:7003/getResource?semester=summer
http://localhost:7003/getResource?semester=winter
 */
app.get("/getResource", (req, res) => {
  const httpNotFoundStatus = 404;
  const httpOKStatus = 200;
  let status;

  if (req.query.semester !== "summer") {
    res.status(httpNotFoundStatus).send(`<h1>Cannot find resource for ${req.query.semester} </h1>`);
  } else {
    res.status(httpOKStatus).send("<h1>Summer resource is available</h1>");
  }
 
});

/* To Test: curl -X POST http://localhost:7003/postSendingEmailAddress -d "email=test@notreal" */
app.post("/sendEmailAddress", (req, res) => {
  res.send(`<h2>Received via post email address ${req.body.email}</h2>`);
});

app.put("/putRequest", (req, res) => {
  res.send("<h2>Received put request</h2>");
});

app.delete("/deleteRequest", (req, res) => {
  res.send("<h2>Received delete request</h2>");
});

app.listen(portNumber);
console.log(`main URL http://localhost:${portNumber}/`);
