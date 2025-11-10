/* Exporting constant and functions individually */
export const DEFAULT_MAX_SCORE = 200;

export function add(...args) {
  console.log("add all: ", args);
  return args.reduce((acc, c) => acc + c);
}

export function multiply(...args) {
  console.log("multiply all: ", args);
  return args.reduce((acc, c) => acc * c);
}