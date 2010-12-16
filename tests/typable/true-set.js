function foo(o, s) /*: {*: Any, #proto: Any; x : True, y : True, z : Int} * ${"x", "y"} -> True */ {
    return o[s];
}
