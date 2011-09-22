function foo(o, s) /*: { *: Int, x: Str, y: Bool, __proto__: Null } * /(x|z)/ -> Str + Int + Undef */ {
    return o[s];
}
