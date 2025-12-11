const path = require("path");
require("dotenv").config({
   path: path.resolve(__dirname, "credentialsDontPost/.env"),
});

const mongoose = require("mongoose");
const Song = require("./model/Song.js");
const Movie = require("./model/Movie.js");

(async () => {
   try {
      await mongoose.connect(process.env.MONGO_CONNECTION_STRING);

      /* Deleting all songs */
      await Song.deleteMany({});
      const songs = await Song.find({});
      console.log("Songs\n", songs);

      /* Deleting all movies */
      await Movie.deleteMany({});
      const movies = await Movie.find({});
      console.log("Movies\n", movies);

      mongoose.disconnect();
   } catch (err) {
      console.error(err);
   }
})();
