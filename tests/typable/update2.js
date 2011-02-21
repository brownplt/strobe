function foo(o, s) /*: {/x/: Int, /y/: Str + Int} * /(x|y)/ -> Undef */ {
    o[s] = 10;
}
