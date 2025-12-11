import React from "react";

function MovieV2() {
   const [title, setTitle] = React.useState("No Title");
   
   const movieTitleHandler = React.useCallback((event) => {setTitle(event.target.value);});

   return (
      <>
         <h2>Movie Form (V2)</h2>
         <input value={title} onChange = {movieTitleHandler} /> 
         <p>Movie's title {title}</p>
      </>
   );
}

export default MovieV2;
