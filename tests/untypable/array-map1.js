function foo(a) /*: Array<Num> -> Array<Bool> */ {
    function tostr(i) /*: Num -> Str */ {
        return String(i);
    }
    return a.map(tostr);
}
