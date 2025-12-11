import React from "react";
import ReactDOM from "react-dom/client";
import GenericOrderedList from "./GenericOrderedList";

const root = ReactDOM.createRoot(document.getElementById("root"));

root.render(
    <>
        <GenericOrderedList title = "First List" listItems={["first", "second"]} />
        <GenericOrderedList title = "Second List" listItems={[10, 30]} />
        <GenericOrderedList listItems={["July", "August"]} />
    </>
);
