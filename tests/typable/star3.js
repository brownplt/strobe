function foo(o, s) /*: { *: Num, x: Str, y: Bool, __proto__: Null } * /(x|z)/ -> Str + Num + Undef */ {
    return o[s];
}
