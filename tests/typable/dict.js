function foo(a, str) /*: {#proto: Object, *:Int, #code: Undef} * Str -> Int + (-> Str) + Undef */ {
    return a[str];
}

foo({a: 5, b: 10},
    "str");
