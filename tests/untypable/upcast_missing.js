function foo() /*: -> Int */ {
    var x = "hello";
    if (typeof x === "number") {
        return x + 100;
    }
    else { 
        return 200;
    }
}
