function foo(o) /*: {/([a-z])/ : Num, /("foo"|("bar"))/: Str} -> {/(([a-y])*)/ : Num} */ {
    return o;
}
