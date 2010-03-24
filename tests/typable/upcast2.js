function foo() /*: -> Int + String */ {
    // Hints bind tighter than other expressions.
    return /**upcast Int + String */ (200 + 300);
}
