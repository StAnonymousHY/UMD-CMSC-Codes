import React from "react";
import ReactDOM from "react-dom/client";
import { createBrowserRouter, RouterProvider } from "react-router-dom";
import Welcome from "./Welcome";

const router = createBrowserRouter([
    {
        path: "/terpSite",
        element: <Welcome />
    }
])

const root = ReactDOM.createRoot(document.getElementById("root"));
/* The URL to try is http://localhost:5173/terpSite */
root.render(
    <RouterProvider router={router} />
);
