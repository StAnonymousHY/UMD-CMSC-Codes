import React from "react";

function FetchImage() {
   const imagesJSON = "https://www.cs.umd.edu/~nelson/classes/resources/cmsc335/images/someImages.json";
   
   const [imageURL, setImageURL] = React.useState("");
   React.useEffect(() => {
      fetch(imagesJSON)
      .then(response => response.json())
      .then(data => setImageURL(data[0].imageURL))
      .catch(err => console.error(err))
   }, []); 

   return (
      <>
         <h2>Image</h2>
         <p>
            JSON {imageURL}
         </p>
      </>
   );
}

export default FetchImage;
