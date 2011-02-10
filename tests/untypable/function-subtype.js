function foo() /*: -> (Int * (Int + Undef) -> Int) */ {
    return function(n) /*: Int -> Int */ { return n; };
}

function foo2() /*: -> (Int * Any -> Int) */ {
    return function(n) /*: Int -> Int */ { return n; };
}
