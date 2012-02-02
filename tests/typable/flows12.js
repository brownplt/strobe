function expectNum(x) /*: Num -> Num */ {
    return x;
}

var x = /*:upcast Undef + Num */ undefined;
var y = expectNum(200);
expectNum(y);
  
