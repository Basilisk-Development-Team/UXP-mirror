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

function lessThan(a, b) {
  return a < b;
}

function lessEqual(a, b) {
  return a <= b;
}

function greaterThan(a, b) {
  return a > b;
}

function greaterEqual(a, b) {
  return a >= b;
}

function notLess(a, b) {
  return !(a < b);
}

function boolStrictEq(a, b) {
  return (a < b) === (b > a);
}

function strictBoolIntEq(a, b) {
  return (a < b) === 1;
}

function strictBoolIntNe(a, b) {
  return (a < b) !== 1;
}

function boolLooseEq(a, b) {
  return (a < b) == 1;
}

function boolLooseNe(a, b) {
  return (a < b) != 1;
}

function bitMix(a, b) {
  return (a & b) ^ (a | 8);
}

function invertBits(a) {
  return ~a;
}

function negate(a) {
  return -a;
}

function leftShift(a, b) {
  return a << b;
}

function rightShift(a, b) {
  return a >> b;
}

function unsignedRightShift(a, b) {
  return a >>> b;
}

function multiply(a, b) {
  return a * b;
}

function multiplyZero(a) {
  return a * 0;
}

function divide(a, b) {
  return a / b;
}

function divideNegativeZero(a) {
  return 0 / a;
}

function remainder(a, b) {
  return a % b;
}

function remainderNegativeZero(a) {
  return a % 2;
}

function uint16Const() {
  return 50000;
}

function uint24Const() {
  return 70000;
}

function popMany(a, b) {
  return (a + 1, b + 2, a + 3, b + 4, a + b);
}

function assignArg(a, b) {
  a = a < b;
  return a;
}

function boolToNumeric(a, b) {
  var flag = a < b;
  return +flag;
}

function nullValue() {
  return null;
}

function voidValue(a) {
  return void a;
}

function unsetLocal() {
  var x;
  return x;
}

function unsetLooseEqNull() {
  var x;
  return x == null;
}

function conditional(a, b) {
  if (a < b)
    return true;
  return false;
}

function logicalOr(a, b) {
  return a || b;
}

function logicalAnd(a, b) {
  return a && b;
}

function logicalBoolOr(a, b) {
  return (a < b) || (b < a);
}

function logicalBoolAnd(a, b) {
  return (a < b) && (b < 10);
}

function minValue(a, b) {
  return a < b ? a : b;
}

function chooseOrder(a, b) {
  return a < b ? true : false;
}

function sumRange(n) {
  var i = 0;
  var total = 0;
  while (i < n) {
    total = total + i;
    i = i + 1;
  }
  return total;
}

for (var i = 0; i < 100; i++) {
  assertEq(add(i, i + 1), (i + i + 1));
  assertEq(withLocal(i, 7), i + 7);
}

assertEq(overflow(2147483647, 1), 2147483648);
assertEq(unsupported(2, 3), 5.5);
assertEq(largeResult(7), 307);
assertEq(negativeResult(7), -293);
assertEq(lessThan(2, 3), true);
assertEq(lessThan(3, 2), false);
assertEq(lessEqual(3, 3), true);
assertEq(lessEqual(4, 3), false);
assertEq(greaterThan(4, 3), true);
assertEq(greaterThan(2, 3), false);
assertEq(greaterEqual(4, 4), true);
assertEq(greaterEqual(2, 3), false);
assertEq(notLess(2, 3), false);
assertEq(notLess(3, 2), true);
assertEq(boolStrictEq(2, 3), true);
assertEq(boolStrictEq(3, 2), true);
assertEq(strictBoolIntEq(2, 3), false);
assertEq(strictBoolIntEq(3, 2), false);
assertEq(strictBoolIntNe(2, 3), true);
assertEq(strictBoolIntNe(3, 2), true);
assertEq(boolLooseEq(2, 3), true);
assertEq(boolLooseEq(3, 2), false);
assertEq(boolLooseNe(2, 3), false);
assertEq(boolLooseNe(3, 2), true);
assertEq(bitMix(6, 3), (6 & 3) ^ (6 | 8));
assertEq(invertBits(6), ~6);
assertEq(negate(3), -3);
assertEq(negate(-4), 4);
assertEq(1 / negate(0), -Infinity);
assertEq(negate(-2147483648), 2147483648);
assertEq(leftShift(3, 2), 12);
assertEq(leftShift(1, 33), 2);
assertEq(rightShift(-8, 1), -4);
assertEq(unsignedRightShift(-1, 1), 2147483647);
assertEq(unsignedRightShift(-1, 0), 4294967295);
assertEq(multiply(6, 7), 42);
assertEq(multiply(-3, 7), -21);
assertEq(multiply(1073741824, 4), 4294967296);
assertEq(multiplyZero(3), 0);
assertEq(1 / multiplyZero(-3), -Infinity);
assertEq(divide(8, 2), 4);
assertEq(divide(-9, 3), -3);
assertEq(divide(7, 2), 3.5);
assertEq(1 / divideNegativeZero(-3), -Infinity);
assertEq(divide(-2147483648, -1), 2147483648);
assertEq(remainder(7, 3), 1);
assertEq(remainder(8, 2), 0);
assertEq(1 / remainderNegativeZero(-4), -Infinity);
assertEq(uint16Const(), 50000);
assertEq(uint24Const(), 70000);
assertEq(popMany(3, 4), 7);
assertEq(assignArg(2, 3), true);
assertEq(assignArg(3, 2), false);
assertEq(boolToNumeric(2, 3), 1);
assertEq(boolToNumeric(3, 2), 0);
assertEq(nullValue(), null);
assertEq(voidValue(7), undefined);
assertEq(unsetLocal(), undefined);
assertEq(unsetLooseEqNull(), true);
assertEq(conditional(2, 3), true);
assertEq(conditional(3, 2), false);
assertEq(logicalOr(0, 7), 7);
assertEq(logicalOr(5, 7), 5);
assertEq(logicalAnd(0, 7), 0);
assertEq(logicalAnd(5, 7), 7);
assertEq(logicalBoolOr(2, 3), true);
assertEq(logicalBoolOr(3, 3), false);
assertEq(logicalBoolAnd(2, 3), true);
assertEq(logicalBoolAnd(3, 2), false);
assertEq(minValue(2, 3), 2);
assertEq(minValue(5, 1), 1);
assertEq(chooseOrder(2, 3), true);
assertEq(chooseOrder(3, 2), false);
assertEq(sumRange(0), 0);
assertEq(sumRange(5), 10);

(function testPropertyWrappers() {
    function getX(obj) { return obj.x; }
    function setX(obj, value) { return obj.x = value; }

    var obj = { x: 7 };
    assertEq(getX(obj), 7);
    assertEq(setX(obj, 42), 42);
    assertEq(obj.x, 42);
})();
