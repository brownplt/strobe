function foo(o) /*: Int + Str -> Int */ {
    if(typeof o === "string") {
        return 0;
    }
    else {
        foo(foo(o));
        return o;
    }
}