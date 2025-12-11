const path = require("path");
require("dotenv").config({
   path: path.resolve(__dirname, "credentialsDontPost/.env"),
});
const mongoose = require("mongoose");

(async () => {
   try {
      await mongoose.connect(process.env.MONGO_CONNECTION_STRING);

      /* Schema defining structure of a song document */
      /* Valid types: String, Number, Date, Buffer, Boolean, Mixed,
      ObjecdtId, Array, Decimal128, Map */
      const songsSchema = new mongoose.Schema({
         title: {
            type: String,
            required: true
         },
         awards: {
            type: Number,
            required: true
         },
         released: {
            type: Date,
            default: () => Date.now(),
            immutable: true
         },
         grammyWinner: {
            type: Boolean,
            required: true,
            default: false
         }
      });

      const Song = mongoose.model("Song", songsSchema);
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
         awards: 100 // remove it and run code
      });

      /* Retrieving all songs */
      const songs = await Song.find({});
      console.log("Songs\n", songs);

      mongoose.disconnect();
   } catch (err) {
      console.error(err);
   }
})();
