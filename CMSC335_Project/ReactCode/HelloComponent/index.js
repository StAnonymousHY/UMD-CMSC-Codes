/* Defines a Hello component */
function Hello({ name }) {
   return <h2>Hola {name}!</h2>;
}

/* index.html has <div id="root"></div> */
const rootElement = document.getElementById("root");
const root = ReactDOM.createRoot(rootElement);
root.render(
   <div>
      <Hello name="Terps" />
      <Hello name="College Park" />
   </div>
);
