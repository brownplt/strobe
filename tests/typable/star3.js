function foo(o, s) /*: {*: Int; x: Str, y: Bool} * /(x|z)/ -> Str + Int + Undef */ {
    return o[s];
}
