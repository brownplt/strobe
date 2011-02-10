function foo(f) /*: ((Str -> Num) & (Num -> Bool)) -> Bool */ {
    return f(5);
}
