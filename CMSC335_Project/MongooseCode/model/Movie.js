const mongoose = require("mongoose");

const moviesSchema = new mongoose.Schema({
   title: {
      type: String,
      required: true
   },
   oscars: {
      type: Number,
      required: true
   }
});

const Movie = mongoose.model("Movie", moviesSchema);
module.exports = Movie;
