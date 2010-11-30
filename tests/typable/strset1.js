function foo(o, s) 
/*: {a: Str, b: Int, #proto: {c: Str, d: {}, #proto: Null, *: Bot, #code: Bot}, *: Bool, #code: Bot} *
    $^{"a", "c"} -> Int + {} + Bool + Undef */
{
    return o[s];
}
