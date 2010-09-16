function foo(o) /*: (trec t . Undef + Str + {"x": Undef + Array<Int>, #proto: Object, *: 't, #code: 't -> 't}) -> (trec t . Undef + Str + {"x": Undef + Array<Int>, #proto: Object, *: 't, #code: 't -> 't}) */ {
    o.foo = o.dosomething();
    return o;
}