function foo(o) /*: { __proto__: {x: Str}, /(x|(y*))/: Int} -> Str + Int */ {
    return o.x;
}
