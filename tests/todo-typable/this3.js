function Point(x,y) /*: constructor Num * Num -> {x:Num, y:Num} */ {
  this.x = x;
  this.y = y;
};
var f = function() /*: [{x:Num,y:Num}] -> Num */ {
    return this.x + this.y;
};
Point.prototype.sum = f;

var p = new Point(10, 20);
p.sum();

var heck = {x:10, y:20, blarg: f};
heck.blarg();
