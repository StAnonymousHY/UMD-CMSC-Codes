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

   console.log(`Clearing collection ${collectionName} from DB ${databaseName}`);
   try {
      await client.connect();
      const database = client.db(databaseName);
      const collection = database.collection(collectionName);

      const result = await collection.drop();
      console.log(`Result: ${result}`);
   } catch (e) {
      console.error(e);
   } finally {
      await client.close();
   }
})();