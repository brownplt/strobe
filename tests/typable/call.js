function foo(f) /*: {#proto: Function, *: Bot, #code: [Int + {}] Str ... -> Bool} -> Bool */ {
    if(typeof f === 'function') {
        return f.call(5, "foo");
    }
    return false;
}