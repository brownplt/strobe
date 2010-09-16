function foo(f) /*: (trec t . Undef + Str + {#proto: Object, *: 't, #code: 't -> 't}) -> 
trec t . Undef + Str + {#proto: Object, *: Undef, #code: 't -> 't} */ {
    f.foo(f);
}