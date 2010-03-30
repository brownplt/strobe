function expectInt(x) /*: Int -> Int */ {
    return x;
}

var x = /*:upcast Void + Int */ undefined;
var y = expectInt(200);
expectInt(y);
  