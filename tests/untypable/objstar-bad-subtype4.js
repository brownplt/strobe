function foo(o,s) /*: {x:Int, #proto: Object, *: Str, #code: Undef} * Str -> (Int + (-> Str) + Str + Undef) */ {
    return o[s];
}

function foo2(o) /*: {x:Int, toString: (-> Str)} -> (Int + (-> Str) + Str + Undef) */ {
    return foo(o, "y");
}

foo2({x:5,y:true});

