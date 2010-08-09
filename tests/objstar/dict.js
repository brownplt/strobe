function foo(a, str) /*: {#proto: Object, *:Int} * Str -> Int + (-> Str) + Undef */ {
    return a[str];
}

foo({a: 5, b: 10, toString: function() /*: -> Str */ { return "String"; }},
    "str");
