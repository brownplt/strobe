function foo(obj) /*: { x : Num } -> Num */ {
    obj.x = 900;
    return obj.x;
}

function bar(obj) /*: { func : Num -> Num } -> Num */ {
    var x = obj.func(10000);
    return x;
}

