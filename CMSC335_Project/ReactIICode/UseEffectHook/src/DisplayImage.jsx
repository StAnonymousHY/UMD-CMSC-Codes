import React from "react";

function DisplayImage({index}) {
   const imagesJSON = "https://www.cs.umd.edu/~nelson/classes/resources/cmsc335/images/someImages.json";
   
   const [imageURL, setImageURL] = React.useState("");
   React.useEffect(() => {
      fetch(imagesJSON)
      .then(response => response.json())
      .then(data => setImageURL(data[index].imageURL))
      .catch(err => console.error(err))
   }, []); 

   return (
      <>
         <h2>Image</h2>
         <p>
            <img src= {imageURL} width="200" height="200" alt="no image"></img>
         </p>
      </>
   );
}

export default DisplayImage;
