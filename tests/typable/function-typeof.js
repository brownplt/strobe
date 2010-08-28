function foo(f) /*: {} + (Num -> Num) -> Num */ {
    if(typeof f === "function") {
        return f(5);
    }
    return 5;
}