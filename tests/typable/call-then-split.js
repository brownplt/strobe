function foo(o) /*: Str + Int -> Int */ {
    if(typeof o === 'number') {
        foo(o);
        return o;
    }
    return 10;
}