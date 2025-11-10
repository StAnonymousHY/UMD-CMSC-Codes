const DEFAULT_MAX_SCORE = 200;

function add(...args) {
  console.log("add all: ", args);
  return args.reduce((acc, c) => acc + c);
}

function multiply(...args) {
  console.log("multiply all: ", args);
  return args.reduce((acc, c) => acc * c);
}

function getMessage() {
  return getActualMessage();
}

function getActualMessage() {
  return "Testudo";
}

/* Not exporting getActualMessage */
export { DEFAULT_MAX_SCORE, add, multiply, getMessage };

/* We can export each one individually 

export function multiply(...args) {
  console.log("multiply all: ", args);
  return args.reduce((acc, c) => acc * c);
}

*/
