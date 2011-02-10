function foo(arr) /*: {#proto: Array, *: Int, #code: Bot} -> Int */ {
    if(arr instanceof Array) {
        return arr["foo" + "bar"];
    }
    else {
        return 5;
    }
}