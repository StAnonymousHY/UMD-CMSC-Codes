const bcrypt = require("bcrypt");

const getHashedPassword = (password, saltRounds) => {
    /* You can see information about the async version of hashSync 
       by typing bcrypt.hash and hovering over it to see documentation. 
       Notice how it returns a promise (async) */
    const salt = bcrypt.genSaltSync(saltRounds); /* If we don't specify it, the default is 10 */
    console.log(`Salt value: ${salt}`);
    return bcrypt.hashSync(password, salt); 
}

const verifyPassword = (password, hashed) => {
    return bcrypt.compareSync(password, hashed);
}

const password = "terps";
const saltRounds = 10; // try 16
const hashedPassword = getHashedPassword(password, saltRounds);
/* Notice how the hashed value has the salt and round values */
/* Notice how console.log can take multiple arguments */
console.log("hashed: ", hashedPassword);
console.log("Correct password provided: ", verifyPassword(password, hashedPassword));
console.log("Correct password provided: ", verifyPassword(password + "umcp", hashedPassword));