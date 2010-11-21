
function foo(a) /*: Array<{x:Int}> -> Array<{x:Int}> */ {
    a.push(5);
    return a;
}
