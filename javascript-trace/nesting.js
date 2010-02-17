var pln = function (s) {
   s = s || "";
   var p = document.createTextNode(s);
   var rezdiv = document.getElementById("rezdiv");
   rezdiv.appendChild(p);
   rezdiv.appendChild(document.createElement('br'));
  };

var simplefunc = function (arg, arg2) {
    return arg + arg2;
  };

var nestedfunc = function (a) {
    var increment =function (a) { return a + 1; };
    a = increment(a); a = increment(a);
    return a;
  };

var doublenest = function (a) {
    var tostr = function (b) {
        var add1 = function (c) {
            return c + 1;
          };
        var addhi = function (c) {
            return c + "hi";
          };
        return addhi("" + add1(b));
      };
    return tostr(a);
  };

var anonInner = function() {
    return (function(a) {
      return (function(b){return b;})(a)+1; })(5);
  };

