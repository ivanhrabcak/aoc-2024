const fs = require("fs");
const main = () => {
  const contents = fs.readFileSync("input.txt").toString();
  const lines = contents.split("\n");

  const pairs = lines
    .map((line) => line.split("   ").map((k) => +k))
    .reduce(
      (acc, [left, right]) => {
        acc[0].push(left);
        acc[1].push(right);
        return acc;
      },
      [[], []]
    )
    .map((l) => {
      l.sort();
      return l;
    });

  console.log(part1(pairs));
  console.log(part2(pairs));
};

const part1 = (pairs) => {
  return pairs[0]
    .map((k, i) => Math.abs(pairs[1][i] - k))
    .reduce((acc, k) => acc + k, 0);
};

const part2 = (pairs) => {
  const rightOccurences = pairs[1].reduce((acc, k) => {
    if (acc[k] === undefined) {
      acc[k] = 1;
    } else {
      acc[k] += 1;
    }

    return acc;
  }, {});

  return pairs[0]
    .map((k) => {
      const occurences = rightOccurences[k];
      if (occurences === undefined) {
        return 0;
      }

      return occurences * k;
    })
    .reduce((acc, k) => acc + k, 0);
};

main();
