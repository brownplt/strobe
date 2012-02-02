// Not immutable, checks both directions
function foo(o) /*: { /(x|y)/ : ! Num } -> { x: Num, y: Num } */  {
    return o;
}
