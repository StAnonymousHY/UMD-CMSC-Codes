
/* A component with parameters */
function Student({ student }) {
  return (
      <span>
      <strong>Student's Info: </strong>
      <em>
        name: {student.name}, 
        email: {student.email}<br />
      </em>
      </span>
  );
}

export default Student;
