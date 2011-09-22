function foo(o) /*: { * : Int, __proto__: BAD } -> Int + Undef */ {
    return o.x;
}
