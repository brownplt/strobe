function foo() /*:-> Undef */ {

  var i /*: upcast Undef + Int */;
  var j = 10, k = 20;
  i = 0;
  while (i < k) {
    while(i < j) { }

  }
}