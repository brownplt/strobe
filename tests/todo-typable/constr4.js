function Point(x, y) /*: constructor (Num * Num -> {x : Num, y : Num}) */ {
    this.x = x;
    this.y = y;
  };

function sumPoint(pt) /*: Point -> Num */ {
  return pt.x + pt.y;
}

var p = new Point(10, 20);
sumPoint(p);

