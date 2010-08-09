var a = 4 + (/*: cheat Num */ "not a num!");

a = a + 4;

var b = "str" + (/*: cheat Str */ 5);

b.charAt(0);

// Doesn't actually give the expression Bool + Num here
var d = (/*: cheat Bool + Num */ {x:5});

// Doesn't work, claims unreachable code
if(typeof d === "boolean") {

}
