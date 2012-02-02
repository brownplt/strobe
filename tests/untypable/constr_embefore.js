function Point(x, y) /*: constructor (Num * Num -> {x : Num, y : Num}) */ {
    this.x = x;
    this.y = y;
  };

var p = new Point(10, 20);
p.sum();

Point.prototype.sum = function() /*: -> Num */ { return this.x + this.y; };

