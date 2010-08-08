function foo(a, str) /*: {#proto: Object, *:Num} *  Str -> Num + (-> Str) + Undef */ {
    return a[str];
}

foo({a: "Entry for a", b: "Entry for b", toString: function() /*: -> Str */ { return "String"; }});
