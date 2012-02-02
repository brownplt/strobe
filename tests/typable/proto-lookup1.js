function foo(o) /*: { __proto__: {x: Str}, /(x|(y*))/: Num} -> Str + Num */ {
    return o.x;
}
