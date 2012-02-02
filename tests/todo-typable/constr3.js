function Point(x, y) /*: constructor (Num * Num -> {x : Num, y : Num}) */ {
    this.x = x;
    this.y = y;
  };

//subtyping =) ?
function sumPoint(pt) /*: {y : Num, x : Num} -> Num */ {
  return pt.x + pt.y;
}
function sumPoint2(pt) /*: {x : Num, y : Num} -> Num */ {
  return pt.x + pt.y;
}

var p = new Point(10, 20);
sumPoint(p);
//sumPoint2(p);
