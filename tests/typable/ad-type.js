function simple(val) 
/*: (trec X . Undef + Num + {#proto: Object, *:'X, #code: Undef}) -> 
(trec X . Undef + Num + {#proto: Object, *:'X, #code: Undef}) */ {
    return val.foo;
}

function harder(val) 
/*: (trec X . Undef + Num + {fld: Str, #proto: Object, *:'X, #code: Undef}) -> 
Undef + Str */ {
    return val.fld;
}
