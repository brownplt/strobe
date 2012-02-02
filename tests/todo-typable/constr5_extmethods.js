function Point(x, y) /*: constructor (Num * Num -> {x : Num, y : Num}) */ {
    this.x = x;
    this.y = y;
  };

var p = new Point(10, 20);

Point.prototype.sum = function() /*: [Point] -> Num */ { return this.x + this.y; };
Point.prototype.RANDOMVALUE = "hi";
//the same point can now be used to sum:
p.sum();

function bob(s) /*: Str -> Str */ { return s + s; };

bob(p.RANDOMVALUE);
