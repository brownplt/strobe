function foo(o, s) /*: {*: Int; /_(.*)/: Bool} * Str -> Bool + Undef */ {
    if(s.charAt(0) === '_') {
        return o[s];
    }
}
