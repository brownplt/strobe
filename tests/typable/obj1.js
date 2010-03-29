function foo(obj) /*: { x : Int } -> Int */ {
    obj.x = 900;
    return obj.x;
}

function bar(obj) /*: { func : Int -> Int } -> Int */ {
    var x = obj.func(10000);
    return x;
}

