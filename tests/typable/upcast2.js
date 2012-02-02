function foo() /*: -> Num + Str */ {
    // Hints bind tighter than other expressions.
    return /*:upcast Num + Str */ (200 + 300);
}
