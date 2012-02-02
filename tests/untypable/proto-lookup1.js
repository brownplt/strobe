function foo(o) /*: { __proto__: { *: Str, __proto__: BAD }, /(xx*)/: Num } -> Num */ {
    return o.x;
}
