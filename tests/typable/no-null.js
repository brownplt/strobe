function bar() /*: -> {x: Int} + Null + Undef */ {
    return null;
}

function foo() /*: -> {x: Int} */ { 
    var o = bar();
    if(o.x) {
        return o;
    }
    return {x: 5};
}