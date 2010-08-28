function foo(o)
/*: {#proto: Object, *: Undef, #code: Num -> Num} -> Num */ {
    if(typeof o === "function") {
        return o(5);
    }
    return 5;
}
