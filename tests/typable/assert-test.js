function bar() /*: -> Int */ {
    return 5;
}

function foo() /*: -> Int */ {
    var x = bar();
    return x;
}