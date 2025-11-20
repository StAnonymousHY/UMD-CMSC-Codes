const url = "https://www.cs.umd.edu/~nelson/classes/resources/cmsc335/EnglishSpanish.json";
fetch(url)
  .then(response => response.json())
  .then(json => console.log(json))
  .catch(error => console.log(`Reporting error: ${error}`));
