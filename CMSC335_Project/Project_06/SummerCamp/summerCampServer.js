// Getting all the modules we need
process.stdin.setEncoding("utf8");
const path = require("path");
const express = require("express");
const bodyParser = require("body-parser");
const app = express();
app.set("view engine", "ejs");
app.set("views", path.resolve(__dirname, "templates"));
app.use(bodyParser.urlencoded({ extended: false }));
require("dotenv").config({
   path: path.resolve(__dirname, "credentials/.env"),
});
const { MongoClient, ServerApiVersion } = require("mongodb");
const { request } = require("http");

// Code for server
if (process.argv.length != 3) {
  process.stdout.write("Usage summerCampServer.js portNumber\n");
  process.exit(1);
}
else if(String(parseInt(process.argv[2])) !== process.argv[2]){
   process.stdout.write("portNumber has to be a number\n");
  process.exit(1);
}

const portNumber = parseInt(process.argv[2]);
app.listen(portNumber);
console.log(`Web server started and running at: http://localhost:${portNumber}`);

const prompt = "Stop to shutdown the server: ";
process.stdout.write(prompt);
process.stdin.on("readable", function () {
    const dataInput = process.stdin.read();
    if (dataInput !== null) {
        const command = dataInput.trim();
        if (command == "stop"){
            process.stdout.write("Shutting down the server\n");
            process.exit(0);
        }
        else{
            process.stdout.write(`Invalid command: ${command}\n`);
        }
    }
    process.stdout.write(prompt);
    process.stdin.resume();
});

app.get("/apply", (request, response) => { response.render("apply"); });

app.post("/processApplication", (request, response) => {
   const body = request.body;
   const now = new Date();
   const variables = {
      name: body.name,
      email: body.email,
      gpa: body.gpa,
      info: body.info,
      date: now.toString()
   };
   insertApplicant(body.name, body.email, body.gpa, body.info);
   response.render("process", variables);
});

app.get("/reviewApplication", (request, response) => { response.render("review"); });

app.post("/processReviewApplication", (request, response) => {
   const body = request.body;
   const now = new Date();
   const result = reviewApplicant(body.email);
   // Because reviewApplicant is wrapped in async, use .then to extract from Promise
   result.then(body => {
      const variables = {
         name: body ? body.Name : "NONE",
         email: body ? body.Email : "NONE",
         gpa: body ? body.GPA : "NONE",
         info: body ? body.Info : "NONE",
         date: now.toString()
      };
      response.render("reviewProcess", variables);
   });
});

app.get("/adminGPA", (request, response) => { response.render("GPA"); });

app.post("/processAdminGPA", (request, response) => {
   const body = request.body;
   const now = new Date();
   const result = applicantGPA(body.gpa);
   // Because applicantGPA is wrapped in async, use .then to extract from Promise
   result.then(body => {
      let tableHTML = "<table border='1'><tr><th>Name</th><th>GPA</th></tr>";
      if(body.length > 0){
         for (let applicant of body){
            tableHTML += `<tr><td>${applicant.Name}</td><td>${applicant.GPA}</td></tr>`
         }
      }
      tableHTML += "</table>"
      const variables = {
         table: tableHTML
      }
      response.render("processGPA", variables);
   });
});

app.get("/adminRemove", (request, response) => { response.render("remove"); })

app.post("/processAdminRemove", (request, response) => {
   const result = adminRemove();
   // Because adminRemove is wrapped in async, use .then to extract from Promise
   result.then(body => {
      const variables = {
         remCnt: body
      }
      response.render("processRemove", variables);
   });
});

app.get("/", (request, response) => { response.render("index"); });

async function insertApplicant(name, email, gpa, info) {
   const uri = process.env.MONGO_CONNECTION_STRING;
   const client = new MongoClient(uri, { serverApi: ServerApiVersion.v1 });
   try {
      await client.connect();
      const database = client.db("CMSC335DB");
      const collection = database.collection("campApplicants");

      const applicant = { Name: name, Email: email, GPA: gpa, Info: info };
      let result = await collection.insertOne(applicant);
      return result;
   } catch (e) {
      console.error(e);
   } finally {
      await client.close();
   }
}

async function reviewApplicant(email){
   const uri = process.env.MONGO_CONNECTION_STRING;
   const client = new MongoClient(uri, { serverApi: ServerApiVersion.v1 });
   try {
      await client.connect();
      const database = client.db("CMSC335DB");
      const collection = database.collection("campApplicants");

      const filter = { Email: email };
      const result = await collection.findOne(filter);
      return result;
   } catch (e) {
      console.error(e);
   } finally {
      await client.close();
   }
}

async function applicantGPA(gpa){
   const uri = process.env.MONGO_CONNECTION_STRING;
   const client = new MongoClient(uri, { serverApi: ServerApiVersion.v1 });
   try {
      await client.connect();
      const database = client.db("CMSC335DB");
      const collection = database.collection("campApplicants");

      const filter = { GPA: { $gte: gpa } };
      const cursor = collection.find(filter);
      result = await cursor.toArray();
      return result;
   } catch (e) {
      console.error(e);
   } finally {
      await client.close();
   }
}

async function adminRemove(){
   const uri = process.env.MONGO_CONNECTION_STRING;
   const client = new MongoClient(uri, { serverApi: ServerApiVersion.v1 });
   try {
      await client.connect();
      const database = client.db("CMSC335DB");
      const collection = database.collection("campApplicants");

      const filter = {};
      const cursor = collection.find(filter);
      const result = await cursor.toArray();
      await collection.drop();
      return result.length;
   } catch (e) {
      console.error(e);
   } finally {
      await client.close();
   }
}
