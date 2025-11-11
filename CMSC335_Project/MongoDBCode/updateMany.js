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

      const newValues = { stars: 20.0 };
      const filter = { year: 1985 };
      console.log(`Updating movies with year ${filter.year} to have ${newValues.stars} stars`);
      const update = { $set: newValues };

      const result = await collection.updateMany(filter, update);
      console.log(`Documents modified: ${result.modifiedCount}`);
   } catch (e) {
      console.error(e);
   } finally {
      await client.close();
   }
})();