function foo(f) /*: (Int -> Int) -> Int */ {

    function bar(f2) /*: (Int * Str -> Int) -> Int */ {
        return f2(5, "foo");
    }

    return bar(f);

}
