function foo(a) /*: Array<Int> -> Array<Str> */ {
    function tostr(i) /*: Int  -> Str */ {
        return String(i);
    }

    return (/*: [Str] */(/*: [Int] */(a.map))) (tostr);
}
