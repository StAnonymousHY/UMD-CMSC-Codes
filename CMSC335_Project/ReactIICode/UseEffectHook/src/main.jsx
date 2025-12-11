import React from "react";
import ReactDOM from "react-dom/client";
import FetchImage from "./FetchImage"
import DisplayImage from "./DisplayImage"

const root = ReactDOM.createRoot(document.getElementById("root"));

root.render(
    <>
    <FetchImage />
    { /* <DisplayImage index="0" /><DisplayImage index="1" /> */ }
    </>
);
