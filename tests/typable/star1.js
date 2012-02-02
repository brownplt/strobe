function foo(o) /*: { * : Num, __proto__: BAD } -> Num + Undef */ {
    return o.x;
}
