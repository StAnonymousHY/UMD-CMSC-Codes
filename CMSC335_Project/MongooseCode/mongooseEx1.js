const path = require("path");
require("dotenv").config({
   path: path.resolve(__dirname, "credentialsDontPost/.env"),
});
const mongoose = require("mongoose");

(async () => {
   /* The connection string must have the name of the database before
      ?retryWrites=true, otherwise the test database in Atlas will be used */
   try {
      await mongoose.connect(process.env.MONGO_CONNECTION_STRING);

      /* Schema defining structure of a song document */
      /* Valid types: String, Number, Date, Buffer, Boolean, Mixed,
      ObjecdtId, Array, Decimal128, Map */
      const songsSchema = new mongoose.Schema({
         title: String,
         awards: Number,
         released: Date,
         grammyWinner: Boolean
      });

      /* Creating a Model what will allow us to complete CRUD operations
      IMPORTANT: The first argument to model() should be the singular
      form of the collection's name (e.g. the collection will be named
      "songs", if you provide "Song"). Moongoose will change the argument
      you provide to model() to plural and lowercase, and use it as the
      collections name */
      const Song = mongoose.model("Song", songsSchema);

      /* Creating a document (instance of Model) */
      const song1 = new Song({
         title: "Hello",
         awards: 2,
         released: new Date(),
         grammyWinner: true
      });

      /* Saving the song */
      await song1.save();

      /* Retrieving all songs */
      let songs = await Song.find({});
      console.log("Songs\n", songs);

      mongoose.disconnect();
   } catch (err) {
      console.error(err);
   }
})();
