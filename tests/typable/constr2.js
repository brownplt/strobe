var lastSum = 5;

function Point(x, y) /*: [ { x: ? Num, y: ? Num } ] Num * Num 
                      -> Undef */ {
    this.x = x;
    this.y = y;
    var addition = this.x + this.y;
    lastSum = addition;
};

var p = new Point(10, 10);
p.x;
p.y;
