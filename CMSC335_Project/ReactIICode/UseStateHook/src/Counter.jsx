import React from "react";

function Counter() {
   const [value, setValue] = React.useState(0);

   return (
      <>
         <h2>Registration Count</h2>
         <button onClick={() => setValue(currentValue => ++currentValue)}>Increase Count</button><br />
         <p>
         <strong>Count: {value}</strong>
         </p>
      </>
   );
}

export default Counter;
