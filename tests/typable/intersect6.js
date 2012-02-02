function foo(f) /*: (Num -> Bool) & (Str -> Null) & (Undef -> Num) -> Num */ {
  return f();
}
