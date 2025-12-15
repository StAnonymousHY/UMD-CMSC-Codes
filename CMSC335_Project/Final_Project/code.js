process.stdin.setEncoding("utf8");
const express = require("express");
const path = require("path");
const bodyParser = require("body-parser");
require("dotenv").config({
   path: path.resolve(__dirname, "credentialsDontPost/.env"),
});
const mongoose = require("mongoose");
const bcrypt = require("bcrypt");
const app = express();
app.set("view engine", "ejs");
app.set("views", path.resolve(__dirname, "templates"));
app.use(bodyParser.urlencoded({ extended: false }));
app.use(express.static(path.resolve(__dirname, "style")));
app.listen(7003);
const TMDB_READ_TOKEN = process.env.TMDB_CONNECTION_STRING;
const getHashedPassword = (password) => {
    const salt = bcrypt.genSaltSync();
    return bcrypt.hashSync(password, salt); 
}
const verifyPassword = (password, hashed) => {
    return bcrypt.compareSync(password, hashed);
}
let currentUser = null;
(async () => {
try {
    await mongoose.connect(process.env.MONGO_CONNECTION_STRING);
} catch (err) {
    console.error(err);
}})();
const usersSchema = new mongoose.Schema({
    email: { type: String, required: true, unique: true},
    password: { type: String, required: true},
    history: { type: [{ keyword: {type: String}, resultNum: {type: Number}}], default: [] }
});
const User = mongoose.model("User", usersSchema);

app.get("/signup", (request, response) => { response.render("signup", {errMsg: ""}); });
app.post("/signup", (request, response) => { 
    const body = request.body;
    const user = new User({
         email: body.email,
         password: getHashedPassword(body.password)
      });
    user.save()
    .then(data => { response.redirect("/"); })
    .catch (err => { response.render("signup", {errMsg: "email arealdy signed up"}); });
});

app.get("/search", (request, response) => { 
    if (!currentUser){
        response.redirect("/");
        return; 
    }
    response.render("search", {tableContent: ""}); 
    });
app.post("/search", (request, response) => { 
    if (!currentUser){
        response.redirect("/");
        return; 
    }
    const body = request.body;
    search(body.movie).then(result => {
        currentUser.history.push({keyword: body.movie, resultNum: result.results.length});
        currentUser.save();
        let HTMLContent = "";
        if (result.results.length > 0){
            HTMLContent += "<br>";
            HTMLContent += "<table border='1'><tr><th>Movie Name</th><th>Release Date</th><th>Popularity</th><th>Overview</th></tr>";
            sorted = result.results.sort((i1, i2) => i2.popularity-i1.popularity);
            for (let i of sorted){
                HTMLContent += `<tr><td>${i.original_title}</td><td>${i.release_date}</td><td>${i.popularity}</td><td>${i.overview}</td></tr>`;
            }
            HTMLContent+="</table>";
        }
        else{
            HTMLContent += "There are no movies associated with your keyword: <br>"+
            `<span class="regularFont">${body.movie}</span><br>` +
            "Try another word! <br>";
        }
        response.render("search", {tableContent: HTMLContent});
    });
});

app.get("/history", (request, response) => { 
    if (!currentUser){
        response.redirect("/");
        return; 
    }
    let HTMLContent = "";
    if (currentUser.history.length > 0){
        HTMLContent += "<table border='1'><tr><th>Search Keyword</th><th>Result Count</th></tr>";
        let cnt = 0;
        for (let i of currentUser.history){
            HTMLContent += `<tr><td>${i.keyword}</td><td>${i.resultNum}</td></tr>`;
            cnt += 1;
            if (cnt >= 20)
                break;
        } 
        HTMLContent += "</table>";
    }
    else{
        HTMLContent = "No history yet, go search something! ";
    }
    response.render("history", {tableContent: HTMLContent});
});
app.post("/history", async (request, response) => { 
    if (!currentUser){
        response.redirect("/");
        return; 
    }
    currentUser.history = [];
    await currentUser.save();
    response.redirect("/history");
});

app.get("/clearCollection", async (request, response) => { 
    await User.deleteMany({}); 
    response.redirect("/");
})

app.get("/", (request, response) => { 
    currentUser = null; 
    response.render("index", {errMsg: ""}); 
});
app.post("/", (request, response) => { 
    const body = request.body;
    User.findOne({ email: body.email })
    .then(data => {
        if(data !== null){
            if(verifyPassword(body.password, data.password)){
                currentUser = data;
                response.redirect("/search");
            }
            else{
                response.render("index", {errMsg: "Incorrect email/password"});
            }
        }
        else{
            response.render("index", {errMsg: "Incorrect email/password"});
        }
    });
});

async function search(query) {
    if (!query) 
        return;
    const url = `https://api.themoviedb.org/3/search/movie?query=${query}&include_adult=false&language=en-US&page=1`;
    const options = {
        method: 'GET',
        headers: {
            accept: 'application/json',
            Authorization: `Bearer ${TMDB_READ_TOKEN}`
        }
    };
    return fetch(url, options)
    .then(res => res.json())
    .then(json => json)
    .catch(err => console.error(err));
}