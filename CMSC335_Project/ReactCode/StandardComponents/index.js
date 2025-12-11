
/* index.html has <div id="root"></div> */
const rootElement = document.getElementById("root");
const root = ReactDOM.createRoot(rootElement);
root.render(
   <div>
      <h1>Header</h1>
      <p>
         This is a paragraph
      </p>
      
      <h2>Unordered List</h2>
      <ul>
         <li>Item #1</li>
         <li>Item #2</li>
      </ul>

      <h2>Ordered List</h2>
      <ol>
         <li>Item #1</li>
         <li>Item #2</li>
      </ol>

      <h2>Table</h2>
      <table border='1'>
         <thead>
            <tr><th>Name</th><th>Age</th></tr>
         </thead>
         <tbody>
            <tr><td>Pete</td><td>40</td></tr>
         </tbody>
      </table>

      <h2>Link</h2>
      <a href="https://umd.edu/">https://umd.edu/</a>
   </div>
);
