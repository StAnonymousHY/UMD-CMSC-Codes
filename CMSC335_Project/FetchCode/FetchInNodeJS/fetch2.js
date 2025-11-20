(async () => {
   const url =
      "https://www.cs.umd.edu/~nelson/classes/resources/cmsc335/EnglishSpanish.json";
   try {
      const response = await fetch(url);
      const json = await response.json();
      console.log(json);
   } catch (error) {
      console.log(`Reporting error: ${error}`);
   }
})();
