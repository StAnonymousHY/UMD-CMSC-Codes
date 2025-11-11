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

      /* Looking for a single movie */
      const movieName = "Batman";
      console.log(`Looking for movie ${movieName}`);
      let filter = { name: movieName };
      result = await collection.findOne(filter);
      console.log(result ? result : `No movie found with name ${movieName}`);

      /* Looking for several movies */
     
      const targetYear = 2000;
      const starsNumber = 2.0;
      console.log(`Looking for movies year >= ${targetYear}, stars == ${starsNumber}`);
      filter = { year: { $gte: targetYear }, stars: { $eq: starsNumber } };
      const cursor = collection.find(filter);

      // Some Additional comparison query operators: $eq, $gt, $lt, $lte, $ne (not equal)
      // Full listing at https://www.mongodb.com/docs/manual/reference/operator/query-comparison/
      result = await cursor.toArray();
      console.log(result);
   } catch (e) {
      console.error(e);
   } finally {
      await client.close();
   }
})();
