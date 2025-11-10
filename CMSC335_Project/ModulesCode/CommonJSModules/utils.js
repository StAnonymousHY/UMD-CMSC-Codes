const DEFAULT_MAX_SCORE = 200;

function add(...args) {
  console.log("add all: ", args);
  return args.reduce((acc, c) => acc + c);
}

function multiply(...args) {
  console.log("multiply all: ", args);
  return args.reduce((acc, c) => acc * c);
}

module.exports = { DEFAULT_MAX_SCORE, add, multiply };
