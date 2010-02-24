/*::
  function pln : ((String + Int) -> Void)
  function simplefunc : (String * Int -> String)
  function nestedfunc : (Int -> Int)
    function increment : (Int -> Int)
  function doublenest : (Int -> String)
    function tostr : (Int -> String)
      function add1 : (Int -> Int)
      function addhi : (String -> String)
  function anonInner : ( -> Int)
    function  : (Int -> Int)
      function  : (Int -> Int)
  function useFunction : (Dom -> Void)
  function loader : ( Dom -> Void)
*/

//type checks
//changes must be made:
//useFunction takes Dom, not Int
//loader has no [Dom] this-type.

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


var useFunction = function(_) {
  pln("HAHAHA");
  pln(simplefunc("1",2));
  pln(nestedfunc(5));
  pln(nestedfunc(6));
  pln(nestedfunc(7));
  pln(doublenest(10));
  pln(anonInner());
};

var loader = function(_) {
  window.setTimeout(useFunction, 600);
};


document.addEventListener("load", loader, true);