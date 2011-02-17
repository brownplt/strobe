function foo(f) /*: (Num -> (Num -> Bool)) & (Num -> (Str -> Null)) -> Null */ {
  return f(5)("a-string");
}
