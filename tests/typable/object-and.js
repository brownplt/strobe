function foo(o) /*: {x:Int} + Int -> Bool */ {
    return typeof o === 'object' && o.x  === 5;
}

