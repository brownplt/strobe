function foo(o, s) /*: {/x/: Int, /y/: Str + Int} * /(x|(y|z))/ -> Undef */ {
    o[s] = 10;
}
