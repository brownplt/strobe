function foo(o,s) /*: {"foo": ${"foo"}, "bar": ${"bar"}, "baz": ${"baz"}, #proto: Null, *: Bot, #code: Bot} * Str -> $ {"bar" , "baz" , "foo"} */ {
    return o[o[s]];
}