import React from "react";

function MovieV1() {
   const [title, setTitle] = React.useState("No Title");

   return (
      <>
         <h2>Movie Form (V1)</h2>
         <input value={title} onChange = {event => setTitle(event.target.value)} /> 
         <p>Movie's title is {title}</p>
      </>
   );
}

export default MovieV1;
