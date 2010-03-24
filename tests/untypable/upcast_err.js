function foo() /*: -> Int + String */ {
    // Hints bind tighter than other expressions. Below, 200 is upcast so
    // we have an error on addition.
    return /**upcast Int + String */ 200 + 300;
}
