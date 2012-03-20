function foo() { return null; }
foo.prototype = { bar: 5 };
// function foo(x) {
//     return x;
// }

// foo(5);


// var CCfile = (/* nsIJSCID */(Components.classes["@mozilla.org/nsIFile;1"]));
// var nsIFile = (/* nsIFile_IID*/(Components.interfaces.nsIFile));

// var f = (/* nsIFile*/(CCfile.createInstance(nsIFile)));

// // should fail!
// //f.create(5, "hello");

// // should pass
// var pbm = 
//     (/* nsIPrivateBrowsingService*/
//         (Components
//          .classes["@mozlla.org/privatebrowsingservice;1"]
//          .getService(Components.interfaces.nsIPrivateBrowsingService)));
// if (pbm.privateBrowsingEnabled) {
//     f.normalize();
// } else {
//     f.create("afile.txt");
// };

