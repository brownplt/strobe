function simple(val) 
/*: (trec X . Undef + Num + {#proto: Object, *:'X}) -> 
(trec X . Undef + Num + {#proto: Object, *:'X}) */ {
    return val.foo;
}

function harder(val) 
/*: (trec X . Undef + Num + {fld: Str, #proto: Object, *:'X}) -> 
Undef + Str */ {
    return val.fld;
}
