function foo(f) /*: (Num -> Bool) & (Str -> Null) & (Undef -> Int) -> Int */ {
  return f();
}
