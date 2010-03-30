function expectInt(x) /*: Int -> Int */ {
    return x;
}

var x = /*:upcast Void + Int */ undefined;
x = expectInt(200);
expectInt(x);
  