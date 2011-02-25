function foo(a) /*: Array<Int> -> Array<Bool> */ {
    function tostr(i) /*: Int -> Str */ {
        return String(i);
    }
    return a.map(tostr);
}
