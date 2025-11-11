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
      const collectionsList = database.listCollections();

      /* To see a second collection, use the Atlas interface 
         and create a collection in the above database */
      console.log("Collections: ");
      const result = await collectionsList.toArray();
      console.log(result);
   } catch (e) {
      console.error(e);
   } finally {
      await client.close();
   }
})();
