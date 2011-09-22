function foo(o) /*: { __proto__: { *: Str, __proto__: BAD }, /(xx*)/: Int } -> Int */ {
    return o.x;
}
