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

      console.log(`Deleting movies with stars <= 2`);
      const filter = { stars: { $lte: 2 } }; // filter = {} deletes them all
      const result = await collection.deleteMany(filter);
      console.log(`Documents deleted ${result.deletedCount}`);
   } catch (e) {
      console.error(e);
   } finally {
      await client.close();
   }
})();