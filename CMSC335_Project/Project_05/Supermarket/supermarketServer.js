process.stdin.setEncoding("utf8");
const fs = require("fs");

if (process.argv.length != 3) {
  process.stdout.write("Usage supermarketServer.js jsonFile");
  process.exit(1);
}

const fileName = process.argv[2];
let content = "";

fs.readFile(fileName, "utf-8", function (err, fileContent) {
   if (err) {
      throw err;
   }
   content = fileContent;
});

const prompt = "Type itemsList or stop to shutdown the server: ";
process.stdout.write(prompt);
process.stdin.on("readable", function () {
    const dataInput = process.stdin.read();
    if (dataInput !== null) {
        const command = dataInput.trim();
        if (command === "itemsList") {
            process.stdout.write(content);
        }
        else if (command == "stop"){
            process.exit(0);
        }
    }
    process.stdout.write(prompt);
    process.stdin.resume();
});
