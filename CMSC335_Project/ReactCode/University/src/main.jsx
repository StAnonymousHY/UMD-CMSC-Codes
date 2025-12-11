import React from "react";
import ReactDOM from "react-dom/client";
import Campus from "./Campus";
import CampusCity from "./CampusCity";
import University from "./University";
import Student from "./Student";
import Faculty from "./Faculty";
import GraduateStudent from "./GraduateStudent";

const root = ReactDOM.createRoot(document.getElementById("root"));

// Uncomment each one at a time 
// root.render(<Campus />);
// root.render(<CampusCity />);
// root.render(<Student student = {{name:"Mary", email:"mary@non"}} />);
// root.render(<GraduateStudent name="Pete" email="mary@non" advisor="Laura" />);
// root.render(<Faculty />);
root.render(<University />);
