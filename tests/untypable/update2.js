function foo(o, s) /*: {/x/: Num, /y/: Str + Num} * /(x|(y|z))/ -> Undef */ {
    o[s] = 10;
}
