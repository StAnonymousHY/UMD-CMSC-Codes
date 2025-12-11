import React from "react";
import ReactDOM from "react-dom/client";
import MovieV1 from "./MovieV1";
import MovieV2 from "./MovieV2";

const root = ReactDOM.createRoot(document.getElementById("root"));

root.render(
    <>      
       <MovieV1 />
       { /* <MovieV2 /> */}
    </>
);
