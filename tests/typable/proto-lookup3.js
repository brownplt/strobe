function foo(o, s) /*: { __proto__: {y: Bool}, x: Int, y: _ } * /(x|y)/ -> Int + Bool */ {
    return o[s];
}
