function Student() {
  this.courses = [];
}
Student.prototype.enroll = function (c) { this.courses.push(c); };

function GradStudent() {
    Student.call(this);
}

GradStudent.prototype = new Student();

const a = new GradStudent();
const b = new GradStudent();

a.enroll("STAT440");

console.log(a.courses);
console.log(b.courses);

console.log(a.courses + `lmao`);