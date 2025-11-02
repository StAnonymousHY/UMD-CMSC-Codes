const express = require("express");
const app = express();
const path = require("path");
const portNumber = 7003;
const bodyParser = require("body-parser");

app.use(bodyParser.urlencoded({ extended: false }));

app.set("view engine", "ejs");
app.set("views", path.resolve(__dirname, "templates"));

/* To Test: curl -X GET http://localhost:7003/getHTML */
app.get("/getHTML", (req, res) => {
  res.send("<h1>Returning HTML</h1>");
});

/* To Test: curl -X GET "http://localhost:7003/getURLParameters?age=15&salary=777" */
/* Notice the " " around the URL, otherwise the parameters will be no be passed correctly. */
/* While using Insomnia, select "Query" (instead of form) to provide the parameters */
app.get("/getURLParameters", (req, res) => {
   res.send(`Received age: ${req.query.age} salary: ${req.query.salary}`);
});

/* To Test: curl -X GET http://localhost:7003/getJSON */
app.get("/getJSON", (req, res) => {
  const student = { name: "Mary", age: 15 };
  res.json(student);
});

/* To Test: curl -X GET http://localhost:7003/getRender */
app.get("/getRender", (req, res) => {
   const variables = { semester: "Summer" };
   res.render("summary", variables);
});

/* To Test: 
      curl -X GET http://localhost:7003/getRouteParameters/spring/CMSC335
      curl -X GET http://localhost:7003/getRouteParameters/winter/CMSC999
   */
app.get("/getRouteParameters/:semester/CMSC:course", (req, res) => {
   res.send(`Received semester: ${req.params.semester}, course: ${req.params.course}`);
});
 
/* To Test: curl -X GET http://localhost:7003/getRedirect */
app.get("/getRedirect", (req, res) => {
   res.redirect("http://www.cs.umd.edu");
});

/* To Test: curl -X GET http://localhost:7003/getResourceNotFound */
app.get("/getResourceNotFound", (req, res) => {
  const student = { name: "Mary", age: 15 };
  const http_page_file_not_found = 404;
  res.status(http_page_file_not_found);
  res.send("Cannot find resource (e.g., web page or file)");
});

/* To Test: curl -X POST http://localhost:7003/postSendingEmailAddress -d "email=test@notreal" */
app.post("/postSendingEmailAddress", (req, res) => {
  res.send(`<h2>Received via post email address ${req.body.email}</h2>`);
});

/* To Test: curl -X PUT http://localhost:7003/putRequest */
app.put("/putRequest", (req, res) => {
  res.send("<h2>Received put request</h2>");
});

/* To Test: curl -X DELETE http://localhost:7003/deleteRequest */
app.delete("/deleteRequest", (req, res) => {
  res.send("<h2>Received delete request</h2>");
});

app.listen(portNumber);
console.log(`main URL http://localhost:${portNumber}/`);
