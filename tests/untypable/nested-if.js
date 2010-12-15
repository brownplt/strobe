function foo(s) /*: Int + Str -> Str */ {
    if(typeof s === 'string') {
        if(true && false || (true && false)) {
            return s;
        }
    }
}