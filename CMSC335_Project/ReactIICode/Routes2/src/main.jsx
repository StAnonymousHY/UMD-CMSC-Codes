import React from "react";
import ReactDOM from "react-dom/client";
import { createBrowserRouter, Outlet, RouterProvider } from "react-router-dom";
import Welcome from "./Welcome";
import Dorms from "./Dorms";
import Food from "./Food";

const routes = {
    path: "/terpSite",
    element: <Outlet />,
    children: [
        {
            path: "dorms",
            element: <Dorms />
        },
        {
            path: "food",
            element: <Food />
        }
    ]
}
const router = createBrowserRouter([routes])

const root = ReactDOM.createRoot(document.getElementById("root"));
/* URLs to try is http://localhost:5173/terpSite/dorms 
   http://localhost:5173/terpSite/food 
*/
root.render(
    <RouterProvider router={router} />
);
