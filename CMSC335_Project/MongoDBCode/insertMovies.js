const path = require("path");
require("dotenv").config({
   path: path.resolve(__dirname, "credentialsDontPost/.env"),
});
const { MongoClient, ServerApiVersion } = require("mongodb");

async function main() {
   const databaseName = "CMSC335DB";
   const collectionName = "moviesCollection";
   const uri = process.env.MONGO_CONNECTION_STRING;
   const client = new MongoClient(uri, { serverApi: ServerApiVersion.v1 });

   try {
      await client.connect();
      const database = client.db(databaseName);
      const collection = database.collection(collectionName);

      /* Inserting one movie */
      const movie = { name: "Notebook", year: 2000, stars: 2.0 };
      let result = await collection.insertOne(movie);
      console.log(`Inserted Movie ${movie.name}, Movie Id: ${result.insertedId}`);
   
      /* Inserting several movies */
      const moviesArray = [
         { name: "Batman", year: 2021, stars: 1.5 },
         { name: "Wonder Women", year: 2005, stars: 2.0 },
         { name: "When Harry Met Sally", year: 1985, stars: 5 },
         { name: "Hulk", year: 1985, stars: 5 },
      ];
      result = await collection.insertMany(moviesArray);
      console.log(`Inserted ${result.insertedCount} additional movies`);
   } catch (e) {
      console.error(e);
   } finally {
      await client.close();
   }
}

main();
