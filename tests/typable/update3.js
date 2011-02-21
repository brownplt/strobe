function foo(o, s) /*: {/x/: Int, /y/: Str + Int} * /y/ -> Undef */ {
    o[s] = "a string";
}
