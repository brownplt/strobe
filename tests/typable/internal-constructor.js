function foo() /*: -> Num */ {
    function MyConstructor(x) /*: constructor Num -> {x: Num} */ {
        this.x = x;
    }
    var o = new MyConstructor(5);
    return o.x;
}