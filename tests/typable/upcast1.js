function foo() /*: -> Num */ {
    var x = /*:upcast Num + Str */ "hello";
    if (typeof x === "number") {
        return x + 100;
    }
    else { 
        return 200;
    }
}

