var pln = function (s) {
   s = s || "";
   var p = document.createTextNode(s);
   var rezdiv = document.getElementById("rezdiv");
   rezdiv.appendChild(p);
   rezdiv.appendChild(document.createElement('br'));
  };
  
function Point(x, y) {
    this.x = x;
    this.y = y;
  };
Point.prototype.sum = function() {
    return this.x + this.y;
  };
Point.prototype.dubit = function() {
    this.x *= 2;
  };

var usePoints = function() {
    var x = new Point(10, 20); 
    pln(x.x);
    pln(x.y);
    pln(x.sum());
    x.dubit();
    pln(x.sum());

    var y = new Point(x.sum(), 100); 
    pln(y.x);
    pln(y.sum());
    pln(y instanceof Point);
  };

//---------------

function main() {
  pln(new String("HAHAHA"));
  pln(usePoints());
}
