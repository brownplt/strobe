function foo() /*: -> Num + Str */ {
    // Hints bind tighter than other expressions. Below, 200 is upcast so
    // we have an error on addition.
    return /*:upcast Num + Str */ 200 + 300;
}
