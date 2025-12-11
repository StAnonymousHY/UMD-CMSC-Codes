/* A component */
function Faculty() {
   const school = "UMCP";
   const teachers = ["Mary", "John", "Mike"];
   const sayHello = (() => alert("Hello!!!"));

   return (
      <div>
         School name: {school}<br/> { /* Notice the use of braces around school */ }

         Teacher's list:
         { /* We are creating an array of <li> elements */ }
         <ul>
            {teachers.map(teacher => <li>{teacher}</li>)}
         </ul>

         <button onClick={() => alert("Faculty Meeting Day: Friday")}>
            Faculty Meeting Day (click to find out)
         </button>

         <button onClick={sayHello}>Hello Button
         </button>
         <hr/>
      </div>
   );
}

export default Faculty;
