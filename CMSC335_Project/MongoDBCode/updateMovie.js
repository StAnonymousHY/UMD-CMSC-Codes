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

      const targetName = "Batman";
      const newValues = { year: 2022, stars: 7.0 };
      console.log(`Update movie ${targetName}`);
      const filter = { name: targetName };
      const update = { $set: newValues };

      const result = await collection.updateOne(filter, update);
      console.log(`Documents modified: ${result.modifiedCount}`);
   } catch (e) {
      console.error(e);
   } finally {
      await client.close();
   }
})();
