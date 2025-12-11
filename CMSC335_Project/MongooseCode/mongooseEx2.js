const path = require("path");
require("dotenv").config({
   path: path.resolve(__dirname, "credentialsDontPost/.env"),
});
const mongoose = require("mongoose");

(async () => {
   try {
      await mongoose.connect(process.env.MONGO_CONNECTION_STRING);

      const songsSchema = new mongoose.Schema({
         title: String,
         awards: Number,
         released: Date,
         grammyWinner: Boolean
      });

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

      /* We can combine the two previous steps into one */
      await Song.create({
         title: "Raining",
         awards: 3,
         released: new Date(),
         grammyWinner: false
      });

      /* This one does not have all the fields */
      await Song.create({
         title: "Sunshine",
         awards: 100
      });

      /* Retrieving all songs */
      let songs = await Song.find({});
      console.log("Songs\n", songs);

      /* Updating a song's title and printing the songs */
      song1.title = "Hello Friends";
      await song1.save();
      songs = await Song.find({});
      console.log("Songs after updating title\n", songs);

      mongoose.disconnect();
   } catch (err) {
      console.error(err);
   }
})();
