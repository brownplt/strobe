function foo(o, s) /*: {__bah__: Int, _foo_: Str, x : Bool} * /_(.*)_/ -> Int + Str */ {
    return o[s];
}
