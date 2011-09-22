// Not immutable, checks both directions
function foo(o) /*: { /(x|y)/ : ! Int } -> { x: Int, y: Int } */  {
    return o;
}
