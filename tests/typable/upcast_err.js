function foo() /*: -> Int + Str */ {
    // Hints bind tighter than other expressions. Below, 200 is upcast so
    // we have an error on addition.
    return /*:upcast Int + Str */ 200 + 300;
}
