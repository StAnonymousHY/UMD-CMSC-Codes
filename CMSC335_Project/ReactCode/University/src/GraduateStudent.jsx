
/* A component with parameters */
/* Compare with Student.jsx */
function GraduateStudent({ name, email, advisor }) {
  return (
      <span>
      <strong>Graduate Student's Info: </strong>
      <em>
        name: {name}, 
        email: {email}
        advisor: {advisor} <br />
      </em>
      </span>
  );
}

export default GraduateStudent;
