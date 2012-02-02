function f(a, b) /*: Num * Num + Undef -> Num */ {
  if (typeof b != "number") {
    b = 10;
  }
  return a + b;
}

f(10, 100);
f(10, undefined);
f(10);

