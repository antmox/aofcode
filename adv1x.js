#!/usr/bin/env node

// //////////////////////////////////////////////////////////////////////////

'use strict';

// //////////////////////////////////////////////////////////////////////////
//
// 2019 DAY 3
//
// //////////////////////////////////////////////////////////////////////////




// //////////////////////////////////////////////////////////////////////////
//
// 2019 DAY 2
//
// //////////////////////////////////////////////////////////////////////////

function d20021(input) {
    return input.split('\n')
        .map(x => x.match(/[0-9a-z]+/g))
        .filter(x => x)
        .filter(([l, h, c, s]) => {
            let n = [...s].filter(x => x == c).length
            return n >= +l && n <= +h })
        .length;
}

function d20022(input) {
    return input.split('\n')
        .map(x => x.match(/[0-9a-z]+/g))
        .filter(x => x)
        .filter(([l, h, [c], s]) =>
                (s[l - 1] == c) != (s[h - 1] == c))
        .length;
}

// 546
// console.log(d20021(getinput(2002)))

// 275
// console.log(d20022(getinput(2002)))

// console.dir(d20022(getinput(2002)), {maxArrayLength: Infinity})
// console.table(getinput(2002))


// //////////////////////////////////////////////////////////////////////////
//
// 2019 DAY 1
//
// //////////////////////////////////////////////////////////////////////////

function d20011(input) {
    input = input.split('\n').map(Number);

    for (let [a, b] of combinations(input, 2))
        if (a + b == 2020) return (a * b);
}

function d20012(input) {
    input = input.split('\n').map(Number);

    for (let [a, b, c] of combinations(input, 3))
        if (a + b + c == 2020) return (a * b * c);

    // return Array.from(combinations(input, 3))
    //     .filter(([a, b, c]) => (a + b + c == 2020))
    //     .map(([a, b, c]) => (a * b * c)).find(_ => true)
}

// 605364
// console.log(d20011(getinput(2001)))

// 128397680
// console.log(d20012(getinput(2001)))


// //////////////////////////////////////////////////////////////////////////
// //////////////////////////////////////////////////////////////////////////


// https://lowrey.me/es6-javascript-combination-generator
function* combinations(elements, length) {
  for (let i = 0; i < elements.length; i++) {
      if (length === 1)
          yield [elements[i]];
      else {
          let remaining = combinations(
              elements.slice(i + 1, elements.length), length - 1);
          for (let next of remaining)
              yield [elements[i], ...next];
      }
  };
}


// //////////////////////////////////////////////////////////////////////////

function getinput(num) {
    const fs = require('fs');
    return fs.readFileSync('inputs/' + num + '.in', 'utf8')
}


// //////////////////////////////////////////////////////////////////////////

