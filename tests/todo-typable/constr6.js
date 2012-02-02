function Point(x) /*: constructor (Num -> {x : Num}) */ {
    this.x = x;
  };

//you can only call this function with a point once we add the right field...
function sumPoint(pt) /*: {x : Num, y : Num} -> Num */ {
  return pt.x + pt.y;
}

sumPoint({x:23, y:24});

var p = new Point(10);

Point.prototype.y = 12;
p.y;
sumPoint(p);
