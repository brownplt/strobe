function foo(a, str) 
        /*: {#proto: Object, *:Int, #code: Undef} * Str -> 
            Int + (Str -> Bool) + (-> Str) + Undef */ {
    return a[str];
}

