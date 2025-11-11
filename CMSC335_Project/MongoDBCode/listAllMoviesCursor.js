const path = require("path");
require("dotenv").config({
   path: path.resolve(__dirname, "credentialsDontPost/.env"),
});
const { MongoClient, ServerApiVersion } = require("mongodb");

/* Using IIFE instead of main() */
(async () => {
   const databaseName = "CMSC335DB";
   const collectionName = "moviesCollection";
   const uri = process.env.MONGO_CONNECTION_STRING;
   const client = new MongoClient(uri, { serverApi: ServerApiVersion.v1 });

   try {
      await client.connect();
      const database = client.db(databaseName);
      const collection = database.collection(collectionName);

      /* Listing all movies */
      let cursor = collection.find({});
      let document = await cursor.next();
      while (document != null) {
         console.log(`${document.name}, ${document.year}`);
         document = await cursor.next();
      }

      /* Sorting */
      console.log("***** Sorting by year *****");
      cursor = collection.find({}).sort({year: 1}); // 1 ascending, -1 descending
      document = await cursor.next();
      while (document != null) {
         console.log(`${document.name}, ${document.year}`);
         document = await cursor.next();
      }
   } catch (e) {
      console.error(e);
   } finally {
      await client.close();
   }
})();