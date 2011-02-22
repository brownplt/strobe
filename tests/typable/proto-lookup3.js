function foo(o, s) /*: {proto: {y: Bool}; x: Int} * /(x|y)/ -> Int + Bool */ {
    return o[s];
}
