function foo() /*: -> Int */ {
    var x = /*:upcast Int + Str */ "hello";
    if (typeof x === "number") {
        return x + 100;
    }
    else { 
        return 200;
    }
}

