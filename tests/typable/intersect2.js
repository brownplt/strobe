function foo(f) /*: ((Num -> Bool) & (Str -> Null)) -> Null */ {
  return f("a-string");
}

