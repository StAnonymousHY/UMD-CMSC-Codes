const mongoose = require("mongoose");

const songsSchema = new mongoose.Schema({
   title: {
      type: String,
      required: true,
   },
   awards: {
      type: Number,
      required: true,
   },
   released: {
      type: Date,
      default: () => Date.now(),
      immutable: true,
   },
   grammyWinner: {
      type: Boolean,
      required: true,
      default: false,
   }
});

const Song = mongoose.model("Song", songsSchema);
module.exports = Song;
