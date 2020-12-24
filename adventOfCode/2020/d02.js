const fs = require("fs");
const assert = require("assert");

const input = fs.readFileSync("d02.txt", "utf8").split("\n");

function parse_line(input_line) {
  const regex = /^([0-9]+)\-([0-9]+) ([a-z]): ([a-z]+)$/;
  const found = input_line.match(regex);
  return {
    lower: Number(found[1]),
    upper: Number(found[2]),
    character: found[3],
    password: found[4],
  };
}

function solve(validator) {
  let number_of_valid_passwords = 0;
  input.forEach((pw) => {
    if (validator(parse_line(pw))) {
      number_of_valid_passwords += 1;
    }
  });
  return number_of_valid_passwords;
}

function is_valid_pw_part1({ lower, upper, character, password }) {
  let counter = 0;
  for (let i = 0; i < password.length; i += 1) {
    if (password[i] === character) {
      counter += 1;
    }
  }
  return counter >= lower && counter <= upper;
}

assert.strictEqual(solve(is_valid_pw_part1), 519);

function is_valid_pw_part2({ lower, upper, character, password }) {
  return (
    (password[lower - 1] === character) !== (password[upper - 1] === character)
  );
}

assert.strictEqual(solve(is_valid_pw_part2), 708);
