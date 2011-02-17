function foo(f) /*: (Num -> (Num -> Bool)) & (Num -> (Str -> Null)) -> Bool */ {
  return f(5)(5);
}
