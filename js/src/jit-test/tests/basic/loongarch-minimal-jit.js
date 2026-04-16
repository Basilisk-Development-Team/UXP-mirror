function add(a, b) {
  return a + b;
}

function withLocal(a, b) {
  var c = a + b;
  var d = c - 1;
  return d + 1;
}

function overflow(a, b) {
  return a + b;
}

function unsupported(a, b) {
  return a + (b + 0.5);
}

for (var i = 0; i < 100; i++) {
  assertEq(add(i, i + 1), (i + i + 1));
  assertEq(withLocal(i, 7), i + 7);
}

assertEq(overflow(2147483647, 1), 2147483648);
assertEq(unsupported(2, 3), 5.5);
