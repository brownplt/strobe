//__typedJsVars["\"flapjax.js\" (line 38, column 13)"]

this.$jstraceid = "$global"; //mark the global object so we don't deeply inspect it

//an rttype (run time type) is an object of the form:
//{kind: the kind of the type which is one of object_ref; function_ref; object; function; flat; or undefined,
// type: the type information}
//a union-rttype is an array of rttypes
//a named rttype is an object: {name: var name if any, rttype: the rttype of the var}

//type varies based on kind:
//for flat, type is a string from typeof
//for object, it is an object mapping property names to rttypes
//for function, it is an object:
//  {args: an array of union-rttypes indicating the rttypes of the arguments
//   ret: the union-rttype of the ret type
///  thist: the union-rttype of the 'this' type,
//   nested: an array of named rttypes representing local vars/local funcs in order of appearance}
//
//objects and functions also have a 'seen' field which is true if we've seen the object while
//  trying to find out its type (meaning we have a recursive type)

//for object_ref or function_ref, type is a string representing the type alias. the type it
//refers to can be found in __typedJsTypes.

var __typedjs = (function() {  //lambda to hide all these local funcs
  //-------------------
  //stuff for outputting:
  var mktext = function (win, s) {
    return win.win.document.createTextNode(s);
  };
  var mkbr = function (win) {
    return win.win.document.createElement('br');
  };
  var app = function (win, x) { return win.div.appendChild(x); };
  var println = function (win, s) {
    for (var i=0; i < win.level; i++) {
      s = "  " + s;
    }
    app(win, mktext(win, s)); app(win, mkbr(win));
  };
  var clearwin = function (win) {
    while (win.div.hasChildNodes())
      win.div.removeChild(win.div.firstChild);
  };

  var mkwin = function (title) {
    var res = window.open(
      "", title,
      "width=640,height=480,scrollbars=yes");

    //for firefox: 'zero' is doctype, '1' is the htmlelement
    var twbody = res.document.childNodes.item(1).childNodes.item(1);
    while (twbody.hasChildNodes()) twbody.removeChild(twbody.firstChild);
    var resdiv = res.document.createElement("pre");
    resdiv.innerHTML = "Tracing initialized! Results come soon";
    twbody.appendChild(resdiv);

    return {win:res, div: resdiv, level: 0};
  };
  var __alertcount = 0;
  var __tracewin = mkwin(document.title + " traced types");
  var __dbgwin = mkwin(document.title + " DEBUG INFO");
  var debug = function(s) {
    __alertcount += 1;
    //if (__alertcount % 10 == 0) alert("-");
    return println(__dbgwin, s);
  };
  var incdbg = function() { __dbgwin.level += 1; };
  var decdbg = function() { __dbgwin.level -= 1; };

  //-------------------
  //important variables:
  var __typedJsTypes = {$global: {kind: 'object', type: {}}}; //map type aliases to their rttypes
  var __orderedVars = []; //map order of appearance to named rttypes

  //-------------------
  //helper functions
  var symnum = 0;
  var gensym = function(name) {
    if (name) {
      return "$" + name + (symnum++);
    }
    return "$gen" + (symnum++);
  };
  var arrayContains = function(arr, val) {
    for (var i = 0; i < arr.length; i++) {
      if (rtequal(arr[i], val)) { return true; }
    }
    return false;
  };

  var copyFrom = function(b, a) { //copy everything from b to a
    for (var prop in b) {
      a[prop] = b[prop];
    }
  };

  //pjs value --> rttype (as described above)
  var rttype = function(rtval) {
    if (rtval === null) return {kind: 'flat', type: 'null'};
    var res = typeof(rtval);
    if (res == "object") {
      if (rtval instanceof Node) return {kind: 'flat', type: 'DOM'};
      if (rtval.hasOwnProperty("$jstraceid")) {
        return {kind: 'object_ref', type: rtval.$jstraceid};
      }

      var traceid = gensym("obj");
      var typeObj = {};

      //mark the object
      try {
        rtval.$jstraceid = traceid;
        for (var p in rtval) {
          if (p == "$jstraceid") continue;
          //pln("((SEEING PROPERTY: " + p + "))");
          try {
            var property = rtval[p];
          } catch (errorrrr) {
            //'security exception' with skype or something
            var property = undefined;
          }
          var innerres = reffed_rttype(property);
          typeObj[p] = innerres;
        }
      }
      catch (errorr) {
        //various possibilities:
        //"security error" with them not wanting you to enumerate an object
        //inability to set $jstraceid on a native object..
        //dnno what to do
        typeObj["$js_error"] = "cannot inspect object";
      }

      var resRttype = {kind:'object', type:typeObj, seen: false};
      __typedJsTypes[traceid] = resRttype;
      return resRttype;
    }
    else if (res == "function") {
      //handled with the tracing
      return {kind: 'function_ref', type: rtval.$jstraceid};
    }
    else if (res == "number") {
      if (Math.round(rtval) == rtval)
        return {kind:'flat', type:'int'};
      else
        return {kind:'flat', type:'double'};
    }

    return {kind:'flat', type:res};
  };

  //return the rttype, and return a reference to a type if given
  //an object or a function
  var reffed_rttype = function (rtval) {
    var rtt = rttype(rtval);
    if (rtt.kind == "object")
      return {kind: 'object_ref', type: rtval.$jstraceid};
    if (rtt.kind == "function")
      return {kind: 'function_ref', type: rtval.$jstraceid};
    return rtt;
  };

  //return whether two rttypes are equal
  var rtequal = function(rt1, rt2) {
    if (rt1 == rt2) return true;
    if (rt1===undefined && rt2===undefined) return true;
    if (rt1===undefined || rt2===undefined) return false;

    if (rt1.kind != rt2.kind) return false;

    if (rt1.kind == "flat") {
      return rt1.type == rt2.type;
    }

    if (rt1.kind == "object") {
      var allInTwo=true,allInOne=true;
      for (var p in rt1.type) {
        if (!rtequal(rt1.type[p],rt2.type[p])) {
          allInTwo = false;
          break;
        }
      }
      for (var p in rt2.type) {
        if (!rtequal(rt2.type[p],rt1.type[p])) {
          allInOne = false;
          break;
        }
      }
      return allInTwo && allInOne;
    }
    if (rt1.kind == "function_ref" || rt1.kind == "object_ref")
      return rt1.type == rt2.type;

    return false;
  };

  //rttype -> string functions:
  //convert a union-rttype to a str representing the union
  //return: {answer: the str, typesSeen: the types seen in the union}
  var strUnion = function(u, dbg) {
    if (dbg) debug("strUnion of len " + u.length);
    incdbg();
    if (u.length == 1) {
      var res = strType(u[0], dbg);
      decdbg();
      return res;
    }

    var segmentsSeen = [];
    var needComma = false;
    var elementsIn = 0;
    var res = "";
    var typesSeen = {};

    for (var j=0; j < u.length; j++) {
      var innard = strType(u[j], dbg);
      var s = innard.answer;
      if (!arrayContains(segmentsSeen, s)) {
        if (needComma)
          res += ", ";
        res += s;
        elementsIn += 1;
        needComma = true;
        segmentsSeen.push(s);
        copyFrom(innard.typesSeen, typesSeen);
      }
    }
    if (elementsIn != 1) res = "U(" + res + ")";
    decdbg();
    return {answer: res, typesSeen: typesSeen};
  };

  var flatOrRef = function(t) {
    if (t === undefined) return true;
    if (t.kind == "flat" ||
        t.kind == "object_ref" ||
        t.kind == "function_ref")
      return true;
    return false;
  };
  var _quickStrType = function(t) {
    //return brief description of the rttype
    if (t === undefined) return "ERRUNDEF";
    if (flatOrRef(t))
      return t.kind + ", " + t.type;
    return t.kind;
  };

  //input: rttype
  //output: object where
  //{answer: the string result of the answer,
  // typesSeen: the names of the rttypes seen while iterating. needed to know whether
  //   to put a 'rec .' in}
  //don't go into nested functions
  var strType = function(t, dbg) {
    incdbg(); Node;
    if (dbg) debug("strType " + _quickStrType(t));
    var typesSeen = {};
    if (t === undefined) {
      decdbg();
      return {answer: "any", typesSeen: typesSeen}; //no info available
    }

    if (t.kind == "flat") {
      decdbg();
      return {answer: t.type, typesSeen: typesSeen};
    }

    if (t.kind == "object") {
      var res = "{";
      var needComma = false;
      for (var p in t.type) {
        if (needComma) res += ", ";
        needComma = true;
        if (dbg) {
          if (flatOrRef(t.type[p])) {
            debug("field " + p + ": " + _quickStrType(t.type[p]));
          }
          else
            debug("field " + p + "...");
        }
        var innard = strType(t.type[p], dbg && !(t.type[p].kind == "flat"));
        res += p + " :: " + innard.answer;
        copyFrom(innard.typesSeen, typesSeen);
      }
      decdbg();
      return {answer: res + "}", typesSeen: typesSeen};
    }

    if (t.kind == "function") {
      var res = "(";
      if (t.type.thist) {
        if (dbg) debug("function this type...");
        var innard = strUnion(t.type.thist, dbg);
        var strthist = innard.answer;
        if (!(strthist == "{}"))
            res += "[" + strthist + "] ";
        copyFrom(innard.typesSeen, typesSeen);
      }

      if (t.type.args) {
        for (var i=0; i < t.type.args.length; i++) {
          if (dbg) debug("function arg #" + i + "...");
          var innard = strUnion(t.type.args[i], dbg);
          res += innard.answer;
          if (i != t.type.args.length-1)
            res += ", ";
          copyFrom(innard.typesSeen, typesSeen);
        }
      }
      else
        res += "any..."; //no info available

      res += " -> ";
      if (t.type.ret === undefined)
        res += "any";
      else
      {
        if (dbg) debug("function ret type...");
        var innard = strUnion(t.type.ret, dbg);
        res += innard.answer;
        copyFrom(innard.typesSeen, typesSeen);
      }
      decdbg();
      return {answer: res + ")", typesSeen: typesSeen};
    }

    if (t.kind == undefined) {
      decdbg();
      return {answer: "undefined", typesSeen: typesSeen};
    }

    if (t.kind == "function_ref" || t.kind == "object_ref") {
      decdbg();
      typesSeen[t.type] = true;
      return {answer: t.type, typesSeen: typesSeen};

      var realObj = __typedJsTypes[t.type];
      if (realObj === undefined) {
        decdbg();
        return {answer: "BROKEN " + t.kind, typesSeen: typesSeen};
        //alert(t.type + " not found!");
      }
      typesSeen[t.type] = true;
      if (realObj.seen)
      {
        //we're in a "rec" construct
        decdbg();
        return {answer: t.type, typesSeen: typesSeen};
      }

      realObj.seen = true;
      var innard = strType(realObj, dbg)
      var strRealObj = innard.answer;

      if (t.type in innard.typesSeen)
        var resStr = "rec " + t.type + " . " + strRealObj + "";
      else
        var resStr = strRealObj;

      realObj.seen = false;
      copyFrom(innard.typesSeen, typesSeen);
      decdbg();
      return {answer: resStr, typesSeen: typesSeen};
    }

    decdbg();
    return {answer: "UNKNOWN KIND: " + t.kind, typesSeen: typesSeen};
  };

  //given a named rt type, return an array of strings representing the lines
  //of this type and any nestings it might have, if it's a function.
  var strNestedNamedRttype = function(nrt, dbg) {
    var typesSeen = {};
    if (nrt === undefined) {
      return {answer: ["ERROR: BLANK"], typesSeen: typesSeen};
    }
    var n = nrt.name;
    if (!n) n = "";
    var rt = nrt.rttype;
    var line = "";
    if (rt.kind === "function" || rt.kind === "function_ref") {
      line += "function " + n + "() :: ";
    }
    else if (n) {
      line += n + " :: ";
    }
    else {
      return {answer: ["ERROR: NOT FUNC OR NAMED"], typesSeen: typesSeen};
    }

    var res = strType(rt, dbg);
    var tstr = res.answer;
    line += tstr;
    copyFrom(res.typesSeen, typesSeen);

    var isFunc = false;
    if (rt.kind === "function") {
      isFunc = rt.type;
    }
    else if (rt.kind === "function_ref") {
      isFunc = __typedJsTypes[rt.type].type;
    }

    if (isFunc === false || isFunc.nested.length === 0) {
      return {answer: [line + ";"], typesSeen: typesSeen};
    }

    var res = [line + " {"];

    for (var i=0; i < isFunc.nested.length; i++) {
      var innerRes = strNestedNamedRttype(isFunc.nested[i]);
      for (var j = 0; j < innerRes.answer.length; j++) {
        res.push("  " + innerRes.answer[j]);
      }
      copyFrom(innerRes.typesSeen, typesSeen);
    }
    res.push("};");

    return {answer: res, typesSeen: typesSeen};
  };

  //convert an array of arguments to an array of abstract arguments.
  var arrayToAbstract = function(args) {
    var abstractArgs = [ ];
    for (var i = 0; i < args.length; i++) {
      abstractArgs.push(reffed_rttype(args[i]));
    }
    return abstractArgs;
  };

  //merge an array of possible rttypes with a new rttype.
  var mergeRttypes = function(existing, newone) {
    if (existing === undefined)
      return [newone];

    if (arrayContains(existing, newone))
      return existing;
    existing.push(newone);
    return existing;
  };

  //merge two abstract argument arrays
  //existing is an array of unions of rttypes, args is an array of rttypes
  var processArguments = function(existing, args) {
    if (existing == undefined) {
      existing = [];
      for (var i = 0; i < args.length; i++) {
        existing.push([args[i]]);
      }
      return existing;
    }

    for (var i = 0; i < existing.length; i++) {
      existing[i] = mergeRttypes(existing[i], args[i]);
    }

    for (; i < args.length; i++) {
      //extra arguments are U(undefined)
      existing.push([rttype(undefined), args[i]]);
    }
    return existing;
  };

  //wrap every RHS with this function when assigning:
  /*var tracevar = function(label, v) {
    var traceid = gensym("var");*/


  //wrap every function with this function. it makes
  //every function call add trace information.
  //nester is the function this function is nested in.
  //undefined if top-level. otherwise it should be wrapped
  //name is the name, if any
  //position is what position it occupies in the nester, e.g.
  //0 if the first position, 1 if the 2nd, etc.
  var tracefunction = function(fn, nester, name, position, dbg) {
    var traceid = gensym("func" + (name ? ("_" + (name.substring(name.lastIndexOf(".")+1))) : ""));
    var func_rttype = {
      kind: "function",
      type: {
        args: undefined,
        ret: undefined,
        thist: undefined,
        nested: []},
      seen: false};
    var tjstype = func_rttype.type;
    var ref = {
      kind: "function_ref",
      type: traceid};

    if (nester !== undefined) {
      //insert the func in the proper position in the nester
      __typedJsTypes[nester.$jstraceid].type.nested[position] = {
        name: name, rttype: ref};
    }
    else {
      //insert it in the right place in the global
      __orderedVars[position] = {name: name, rttype: ref};
    }

    var res = function() {
      var calledWithNew = this instanceof arguments.callee;
      if (dbg) debug("traced function called!" + calledWithNew);

      if (!calledWithNew) {
        //update the type of this upon entering the function
        tjstype.thist = mergeRttypes(
          tjstype.thist, reffed_rttype(this));
      }

      tjstype.args = processArguments(
        tjstype.args, arrayToAbstract(arguments));
      if (dbg) debug("done proc args, abt to apply...");
      var r = fn.apply(this, arguments);
      if (dbg) debug("traced function applied! proccing ret...");
      tjstype.ret = mergeRttypes(
        tjstype.ret, reffed_rttype(r));
      if (dbg) debug("procced ret!");
      if (calledWithNew) {
        //update the type of this upon exiting the constructor
        tjstype.thist = mergeRttypes(
          tjstype.thist, reffed_rttype(this));
      }
      return r;
    };
    res.$jstraceid = traceid;
    //give the original function a traceid as well, so that
    //we can pass arguments.callee as the nester
    fn.$jstraceid = traceid;

    __typedJsTypes[traceid] = func_rttype;

    return res;
  };

  //code to update the tracing output:
  var update_tracewin = function() {
    clearwin(__dbgwin);
    clearwin(__tracewin);
    if (__orderedVars.length == 0) {
      println(__tracewin, "No tracing info yet...");
      return;
    }

    println(__tracewin, "/*::");

    var typesSeen = {};

    for (var i=0; i < __orderedVars.length; i++) {
      println(__dbgwin, "stringing var #" + i);
      var res = strNestedNamedRttype(__orderedVars[i], true);
      println(__dbgwin, "done with var #" + i);
      var strs = res.answer;
      copyFrom(res.typesSeen, typesSeen);
      for (var j = 0; j < strs.length; j++) {
        println(__tracewin, "  " + strs[j]);
      }
    }
    println(__tracewin, "*/");

    println(__tracewin, "/*:::");
    var typesPrinted = {};
    while (true) {
      var printedSmth = false;
      var newTs = {};
      for (var ts in typesSeen) {
        if (ts in typesPrinted) continue;
        var res = strType(__typedJsTypes[ts], true);
        println(__tracewin, "  type " + ts + " :: " + res.answer);
        copyFrom(res.typesSeen, newTs);
        typesPrinted[ts] = true;
        printedSmth = true;
      }
      typesSeen = newTs;
      if (!printedSmth) break;
    }
    println(__tracewin, "*/");
  };


  var breathe = function() {
    alert("Breathing...");
  };
  setInterval(update_tracewin, 5000);
  //setInterval(breathe, 1000);

  return tracefunction;
})();


