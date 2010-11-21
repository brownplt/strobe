function foo(o, s) /*: {#proto: Object, *: Int + Undef, #code: Undef} * Str -> Any */ {
    return o[s];
}

foo({x:/*: upcast Int + Undef */ 5, y: /*: upcast Int + Undef */ 6}, "y");
