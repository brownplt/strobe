function foo(o) /*: 
    #{*: Str, /(a*)/: Bool, /b(b*)/: Bool}
->  #{*: Bool + Str, /(a*|b)/: Int + Bool} */ {
    return o;
}
