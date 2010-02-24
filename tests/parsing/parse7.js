function(x) { 
  return { foo: 34 }; // No conflicts!
};

function(x) { 
  // When we read the colon, should we reduce foo as a Prop or a label?
  // We pick label. So, 50 is the start of an ExprStmt, which concludes with
	// the inner semicolon.
  { foo: 50; } 
  ;
};

if (true) {} else {}
