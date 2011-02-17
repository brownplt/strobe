function foo(f) /*: (Str -> Int) & (Num -> Bool) -> Bool */ {
  return f(null);
}
