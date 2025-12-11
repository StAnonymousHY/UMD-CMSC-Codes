import React from "react";

function Movie() {
   const [title, setTitle] = React.useState("No Title");

   return (
      <>
         <h2>Movie Form</h2>
         <input value={title} onChange = {event => setTitle(event.target.value)} /> 
         <p>Movie's title is {title}</p>
      </>
   );
}

export default Movie;
