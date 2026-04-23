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

function largeResult(a) {
  return a + 300;
}

function negativeResult(a) {
  return a - 300;
}

for (var i = 0; i < 100; i++) {
  assertEq(add(i, i + 1), (i + i + 1));
  assertEq(withLocal(i, 7), i + 7);
}

assertEq(overflow(2147483647, 1), 2147483648);
assertEq(unsupported(2, 3), 5.5);
assertEq(largeResult(7), 307);
assertEq(negativeResult(7), -293);

(function testPropertyWrappers() {
    function getX(obj) { return obj.x; }
    function setX(obj, value) { return obj.x = value; }

    var obj = { x: 7 };
    assertEq(getX(obj), 7);
    assertEq(setX(obj, 42), 42);
    assertEq(obj.x, 42);
})();
