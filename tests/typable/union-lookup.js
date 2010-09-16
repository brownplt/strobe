function foo(o) /*: (trec t . Undef + Str + {"x": Undef + Array<Int>, #proto: Object, *: 't, #code: 't -> 't}) -> Int */ {
    return o.x[0];
}