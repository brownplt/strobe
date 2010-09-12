function foo(o, s) /*: {x:Int, #proto:Object, *:Undef, #code:Undef} * Str -> Undef */ {
    o[s] = undefined; // should fail --- what if s === "x"?
}