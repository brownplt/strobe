function foo(a) /*: Array<Num> -> Array<Str> */ {
    function tostr(i) /*: Num  -> Str */ {
        return String(i);
    }

    return (/*: [Str] */(/*: [Num] */(a.map))) (tostr);
}
