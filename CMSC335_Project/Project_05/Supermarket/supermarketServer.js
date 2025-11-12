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

class Body{
    #name;
    #email;
    #delivery;
    #info;

    constructor(name, email, delivery, info){
        this.#name = name;
        this.#email = email;
        this.#delivery = delivery;
        this.#info = info;
    }

    get name(){
        return this.#name;
    }

    get email(){
        return this.#email;
    }

    get delivery(){
        return this.#delivery;
    }

    get info(){
        return this.#info;
    }
}

if (process.argv.length != 3) {
  process.stdout.write("Usage supermarketServer.js jsonFile\n");
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
    let tableHTML = "";
    let totalCost = 0;
    if(request.body.itemsSelected != undefined){
        tableHTML += "<table border='1'><tr><th>Item</th><th>Cost</th></tr>";
        for (let i of request.body.itemsSelected){
            tableHTML += `<tr><td>${content.itemsList[i].name}</td><td>${content.itemsList[i].cost.toFixed(2)}</td></tr>`;
            totalCost += content.itemsList[i].cost;
        }
        tableHTML += `<tfoot><tr><td>Total Cost: </td><td>${totalCost}</td></tr></tfoot></table>`;
    }

    const body = new Body(request.body.name, request.body.email, request.body.delivery, request.body.orderInformation);
    const order = {
        name: body.name,
        email: body.email,
        delivery: body.delivery,
        orderTable: tableHTML,
        orderInformation: body.info
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