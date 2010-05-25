function expectInt(x) /*: Int -> Int */ {
    return x;
}

var x = /*:upcast Undef + Int */ undefined;
var y = expectInt(200);
expectInt(y);
  