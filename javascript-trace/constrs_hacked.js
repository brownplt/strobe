// *** TYPES FOR constrs.js *** 

 
function Point(x, y) /*: constructor (Int * Int -> {x : Int, y : Int}) */ {
    this.x = x;
    this.y = y;
  };
/*Point.prototype.sum = function() {
    return this.x + this.y;
  };
Point.prototype.dubit = function() {
    this.x *= 2;
  };*/

var usePoints = function() /*: ( -> Void) */ {
    /*var x = new Point(10, 20); 
    pln(x.x);
    pln(x.y);*/
    /*pln(x.sum());
    x.dubit();
    pln(x.sum());

    var y = new Point(x.sum(), 100); 
    pln(y.x);
    pln(y.sum());
    pln(y instanceof Point);*/
  };

//---------------

function main(_) /*: (Int -> Void) */ {
  usePoints();
}
