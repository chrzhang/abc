const fs = require("fs");
const assert = require("assert");

const input = fs.readFileSync("d01.txt", "utf8").split("\n");
const input_as_numbers = input.map((e) => Number(e));

function part1() {
  const input_as_set = new Set(input_as_numbers);
  let result = null;
  input_as_set.forEach((e) => {
    const compl = 2020 - e;
    if (input_as_set.has(compl)) {
      result = e * compl;
    }
  });
  return result;
}
assert.strictEqual(part1(), 956091);

function incrLeft(curr, left) {
  left += 1;
  return left === curr ? left + 1 : left;
}

function decrRight(curr, right) {
  right -= 1;
  return right === curr ? right - 1 : right;
}

function part2() {
  const input_sorted = [...input_as_numbers].sort((a, b) => a - b);
  const N = input_sorted.length;
  let curr = 0;
  while (curr < N) {
    let left = -1;
    let right = N;
    left = incrLeft(curr, left);
    right = decrRight(curr, right);
    while (left < right) {
      const three_sum =
        input_sorted[curr] + input_sorted[left] + input_sorted[right];
      if (three_sum > 2020) {
        right = decrRight(curr, right);
      } else if (three_sum < 2020) {
        left = incrLeft(curr, left);
      } else {
        return input_sorted[curr] * input_sorted[left] * input_sorted[right];
      }
    }
    curr += 1;
  }
}
assert.strictEqual(part2(), 79734368);
