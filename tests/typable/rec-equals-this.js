var outside /*:upcast (trec t . Undef + Str + {#proto: Object, *: 't, #code: 't -> 't}) */;

function foo() /*: [(trec t . Undef + Str + {#proto: Object, *: 't, #code: 't -> 't})] -> 
trec t . Undef + Str + {#proto: Object, *: 't, #code: 't -> 't} */ {
    outside = this;
    return this;
}
