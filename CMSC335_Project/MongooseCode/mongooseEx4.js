const path = require("path");
require("dotenv").config({
   path: path.resolve(__dirname, "credentialsDontPost/.env"),
});

const mongoose = require("mongoose");

/* Including Song and Movie */
const Song = require("./model/Song.js");
const Movie = require("./model/Movie.js");

(async () => {
   try {
      await mongoose.connect(process.env.MONGO_CONNECTION_STRING);
      await Song.create({
         title: "The Train",
         awards: 3,
         released: new Date(),
         grammyWinner: false
      });
      await Song.create({
         title: "Parking",
         awards: 100
      });
      await Movie.create({
         title: "The Laptop",
         oscars: 5
      });

      /* Retrieving all songs */
      const songs = await Song.find({});
      console.log("Songs\n", songs);

      /* Retrieving all movies */
      const movies = await Movie.find({});
      console.log("Movies\n", movies);

      mongoose.disconnect();
   } catch (err) {
      console.error(err);
   }
})();
