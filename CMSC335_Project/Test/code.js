// let answer = `Value<strong>${value}</strong><br><em>Sqrt${Math.sqrt(value)}</em>`;

function test2(){
    document.writeln(getList(["apple", "banana"], true));
    document.writeln("Second");
    document.writeln(getList(["apple", "banana"], false));
}

function getList(arr, ordered){
    let answer = "";
    if(ordered){
        answer += "<ol>";
        for(let i = 0 ; i < arr.length ; i++){
            answer += "<li>" + arr[i] + "</li>";
        }
        answer += "</ol>";
    }
    else{
        answer += "<ul>";
        for(let i = 0 ; i < arr.length ; i++){
            answer += "<li>" + arr[i] + "</li>";
        }
        answer += "</ul>";
    }

    return answer;
}

function test1(){
    let data = [10, 3, 89, 5];
    sortNumbers(data, true);
    document.writeln("First " + data.join() + "<br>");
    sortNumbers(data, false);
    document.writeln("Second " + data.join());
    console.log(data[9]);
}

function sortNumbers(data, increasing){
    let index = 0;
    let curr = data[0];
    for(let i = 0 ; i < data.length ; i++){
        for(let j = i+1 ; j < data.length ; j++){
            if((increasing && data[j] < curr) || (!increasing && data[j] > curr)){
                curr = data[j];
                index = j;
            }
        }

        let temp = data[i];
        data[i] = curr;
        data[index] = temp;

        index = i+1;
        curr = data[index];
    }
}

function factorial(){
    document.querySelector("#output").textContent = factHelper(document.querySelector("#factInput").value);
}

function factHelper(num){
    let n = Number.isNaN(Number(num));
    if(n)
        return NaN;
    if(Number(num) <= 0)
        return 1;
    return factHelper(Number(num)-1)*Number(num);
}

function factTable(limit){
    document.writeln("<table border='1'>");
    for(let i = 1 ; i <= limit ; i++)
        document.writeln("<tr><td>" + i + "</td>" + "<td>" + factHelper(i) + "</td></tr>");
    document.writeln("</table>");
}

async function displayComputerTypeInfo(){
    const response = await fetch("./computers.json");
    const data = await response.json();
    const type = document.querySelector("#pcInput").value.toLowerCase();
    let ansArr = [];

    if(type == "any")
        data.forEach((v) => ansArr.push([v.type, v.quantity]));
    else
        data.filter((v) => v.type == type).forEach((v) => ansArr.push([v.type, v.quantity]));

    let answer = "<h3>Computer's Type Info</h3>\n";
    if(ansArr.length == 0){
        document.querySelector("#displayArea").innerHTML = answer + "No computer type found";
        return;
    }

    answer += "<table border='1'><tr style='color: blue;'><th>Type</th><th>Quantity</th></tr>";
    
    for (let i of ansArr)
        answer += `<tr><td>${i[0]}</td><td>${i[1]}</td></tr>`;

    answer += "</table>";

    document.querySelector("#displayArea").innerHTML = answer;
}