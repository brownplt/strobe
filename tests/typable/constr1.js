var lastSum = 5;

function Point(x, y) /*: [ { x: ? Num, y: ? Num } ] Num * Num 
                      -> Undef */ {
    this.x = x;
    this.y = y;
    var addition = this.x + this.y;
    lastSum = addition;
}

