function foo() /*: -> Int + Str */ {
    // Hints bind tighter than other expressions.
    return /*:upcast Int + Str */ (200 + 300);
}
