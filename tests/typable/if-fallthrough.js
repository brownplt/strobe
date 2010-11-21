function foo(x) /*: Num + Str -> Num */ {
    if(typeof x === 'string') {
        return 5;
    }
    return x;
}
