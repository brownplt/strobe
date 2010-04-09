function f(a, b) /*: Int * Int + Void -> Int */ {
  if (typeof b != "number") {
    b = 10;
  }
  return a + b;
}

f(10, 100);
f(10, undefined);
f(10);

