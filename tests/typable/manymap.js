function foo(a) /*: Array<Num> -> Array<Str> */ {
    function tonum(n) /*: Str -> Num */ {
        return Number(n);
    }
    function tostr(n) /*: Num -> Str */ {
        return String(n);
    }
    return a.map(tostr).map(tonum).map(tostr);
}
