function foo(o, s) /*: {__bah__: Int, _foo_: Str} * /_(.*)_/ -> Int + Str */ {
    return o[s];
}