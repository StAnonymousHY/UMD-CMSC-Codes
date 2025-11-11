process.stdin.setEncoding("utf8");
const fs = require("fs");
const path = require("path");
const express = require("express");
const bodyParser = require("body-parser");

const app = express();
const portNumber = 5000;

app.set("view engine", "ejs");
app.set("views", path.resolve(__dirname, "templates"));
app.use(bodyParser.urlencoded({ extended: false }));

if (process.argv.length != 3) {
  process.stdout.write("Usage supermarketServer.js jsonFile");
  process.exit(1);
}

const fileName = process.argv[2];
let content;

fs.readFile(fileName, "utf-8", function (err, fileContent) {
   if (err) {
      throw err;
   }
   content = JSON.parse(fileContent);
});

app.get("/catalog", (request, response) => { 
    let tableHTML = "<table border='1'><tr><th>Item</th><th>Cost</th></tr>";
    for (let i of content.itemsList){
        tableHTML += `<tr><td>${i.name}</td><td>${i.cost.toFixed(2)}</td></tr>`;
    }
    tableHTML += "</table>";
    const table = {
        itemsTable: tableHTML
    };
    response.render("displayItems", table);
});

app.get("/order", (request, response) => { 
    let listHTML = "";
    for (let i = 0 ; i < content.itemsList.length ; i++){
        listHTML += `<option value='${i}'>${content.itemsList[i].name}</option>`
    }
    const list = {
        items: listHTML
    }
    response.render("placeOrder", list);
});

app.post("/order", (request, response) => {
    let tableHTML = "<table border='1'><tr><th>Item</th><th>Cost</th></tr>";
    let totalCost = 0;
    for (let i of request.body.itemsSelected){
        tableHTML += `<tr><td>${content.itemsList[i].name}</td><td>${content.itemsList[i].cost.toFixed(2)}</td></tr>`;
        totalCost += content.itemsList[i].cost;
    }
    tableHTML += `<tfoot><tr><td>Total Cost: </td><td>${totalCost}</td></tr></tfoot></table>`;
    const order = {
        name: request.body.name,
        email: request.body.email,
        delivery: request.body.delivery,
        orderTable: tableHTML,
        orderInformation: request.body.orderInformation
    }
    response.render("orderConfirmation", order);
});

app.get("/", (request, response) => { response.render("index"); });


app.listen(portNumber);
console.log(`To access server: http://localhost:${portNumber}`);

const prompt = "Type itemsList or stop to shutdown the server: ";
process.stdout.write(prompt);
process.stdin.on("readable", function () {
    const dataInput = process.stdin.read();
    if (dataInput !== null) {
        const command = dataInput.trim();
        if (command === "itemsList") {
            console.log(content.itemsList);
        }
        else if (command == "stop"){
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