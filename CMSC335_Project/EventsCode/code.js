
/* No script tags used; just JavaScript */
document.querySelector("#displayTable").onclick = displaySqrts;

function displaySqrts() {
     let usingInnerHTML = true;  // Try true

    const maximum = Number(document.querySelector("#myTextField").value);

    let answer = "<table border='1'>";
    answer += "<tr><th>Value</th><th>Sqrt</th></tr>"
    for (let i = 1; i <= maximum; i++) {
        answer += `<tr><td>${i}</td><td>${Math.sqrt(i)}</td></tr>`;
    }
    answer += "</table>";

    if (usingInnerHTML) {
        document.querySelector("#displayArea").innerHTML = answer;
    } else {
        document.writeln(answer);
    }
}