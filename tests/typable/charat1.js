function foo(o, s) /*: {*: Num, /_(.*)/: Bool} * Str -> Bool + Undef */ {
    if(s.charAt(0) === '_') {
        return o[s];
    }
}
