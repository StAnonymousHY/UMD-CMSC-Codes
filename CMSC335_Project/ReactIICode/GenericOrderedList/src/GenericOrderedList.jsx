
function GenericOrderedList({title = "No title", listItems}) {
   return (
      <>
         <h2>{title}</h2>
         <ol>
            {listItems.map(item => <li>{item}</li>)}
         </ol>
      </>
   );
}

export default GenericOrderedList;
