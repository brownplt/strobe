function foo(o) /*: {proto: {x: Str}; /(x|(y*))/: Int} -> Str + Int */ {
    return o.x;
}