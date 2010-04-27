//names in the paper were changed to be clearer:
//wrapFunc is actually 'tracefunction' here, which becomes '__typedjs' to the outside
//inspectValue is really the function 'rttype'.
//in rttype, the field "info" is really the field "type".
//flat "undefined" doesnt exist, but is called "Void"

//__typedJsVars["\"flapjax.js\" (line 38, column 13)"]

var $jstraceid = "Global"; //mark the global object so we don't deeply inspect it

//an rttype (run time type) is an object of the form:
//{kind: the kind of the type which is one of object_ref; function_ref; object; function; flat; or undefined,
// type: the type information}
//a union-rttype is an array of rttypes
//a named rttype is an object: {name: var name if any, rttype: the rttype of the var}

//type varies based on kind:
//for flat, type is a string from typeof
//for object, it is an object mapping property names to rttypes.
//            has a field "$isArray", true if it's an array
//            if is array, then "$itemType" is union-rttype of the item types.
//for function, it is an object:
//  {args: an array of union-rttypes indicating the rttypes of the arguments
//   ret: the union-rttype of the ret type
///  thist: the union-rttype of the 'this' type,
//   nestingLevel: nesting level the func appears in, for prettier output}
//
//objects and functions also have a 'seen' field which is true if we've seen the object while
//  trying to find out its type (meaning we have a recursive type)

//for object_ref or function_ref, type is a string representing the type alias. the type it
//refers to can be found in __typedJsTypes.

var holder = (function() {  //lambda to hide all these local funcs
  //-------------------
  //if we're on google gadgets, we have to do things differently
  //this flag lets us know:
  var isGG = (typeof navigator) === "undefined";
  
  //stuff for outputting
  var __windows = [];
  //these just modify what the window contents should show, but
  //don't actually show anything:
  var println = function (win, s) {
    for (var i=0; i < win.level; i++) {
      s = "  " + s;
    }
    win.data.push(s);
  };
  var clearwin = function (win) {
    win.data.splice(0, win.data.length);
  };

  var mkwin = function (title) {
    var res = {title: title, level: 0, data: []};
    
    if (!isGG) {
      var win = window.open(
        "", title,
        "width=640,height=480,scrollbars=yes");

      //for firefox: 'zero' is doctype, '1' is the htmlelement
      var twbody = win.document.childNodes.item(1).childNodes.item(1);
      while (twbody.hasChildNodes()) twbody.removeChild(twbody.firstChild);
      var resdiv = win.document.createElement("pre");
      resdiv.innerHTML = "Tracing initialized! Results come soon";
      twbody.appendChild(resdiv);
      res.win = win;
      res.div = resdiv;
    }
    
    __windows.push(res);
    
    return res;
  };  
  
  //this shows the window.
  var showwin = function (win) {
    //in GG it pops up the detail view
    if (isGG) {
      var detailsView = new DetailsView();
  
      // Create the details view and set its content.
      detailsView.SetContent(
        "",               // Item's displayed website/news source.
        undefined,        // Time created
        "traceview.xml",  // The XML file
        false,            // Whether time is shown as absolute time
        0);               // Content layout flags

      var refs = {__typedJsTypes: __typedJsTypes, __orderedVars: __orderedVars,
                  win: win};
      detailsView.detailsViewData.putValue("refs", refs);
      
      plugin.showDetailsView(
        detailsView,  // The DetailsView object
        win.title,      // The title
        gddDetailsViewFlagNone,  // Flags
        function(){});  // The handler to call when details view closes    
      return;
    }

    //in FF it creates DOM elements to show stuff
    while (win.div.hasChildNodes()) {
      win.div.removeChild(win.div.firstChild);
    };
    
    for (var i=0; i < win.data.length; i++) {
      win.div.appendChild(win.win.document.createTextNode(win.data[i]));
      win.div.appendChild(win.win.document.createElement('br'));
    }
  };
  
  var __tracewin = mkwin(" traced types");
  var __outwin = mkwin("tobestitched");

  var __dbgwin = mkwin(" DEBUG INFO");
  var debug = function(s) {
    //if (__alertcount % 10 == 0) alert("-");
    return println(__dbgwin, s);
  };
  var incdbg = function() { __dbgwin.level += 1; };
  var decdbg = function() { __dbgwin.level -= 1; };

  //-------------------
  //important variables:
  var __typedJsTypes = {$global: {kind: 'object', type: {}}}; //map type aliases to their rttypes
  var __orderedVars = {}; //map file names to a mapping of order of appearance to named rttypes

  //-------------------
  //helper functions
  var Object_hasOwnProperty = function(obj, prop) {
    if (isGG) {
      //apparently not all objects support "in"...
      try {
        return prop in obj;
      }
      catch (e) {
        return false;
      }
    }
    return obj.hasOwnProperty(prop);
  }
  var symnum = 0;
  var gensym = function(name) {
    if (name) {
      return name + (symnum++);
    }
    return "gen" + (symnum++);
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
    if (rtval === null) return {kind: 'flat', type: 'Null'};
    var res = typeof(rtval);
    if (res == "object") {
      if (!isGG) {
        if (rtval instanceof Node) {
          return {kind: 'flat', type: 'Unknown'};
        }
        if (rtval instanceof Event) {
          return {kind: 'flat', type: 'Unknown'};
        }
      }
      if (rtval instanceof String) {
        //indistinguishable from the primitive string:
	return {kind:'flat', type:'String'};
      }
      if (rtval instanceof Number) {
        return {kind:'flat', type:'Number'};
      }
      if (rtval instanceof Date) {
        return {kind:'flat', type:'Date'};
      }
      //check for pre-traced objects:
      if (Object_hasOwnProperty(rtval, "$jstraceid")) {
        if (rtval.$jstraceid == "Global")
          return {kind: 'flat', type: "Global"};
        return {kind: 'object_ref', type: rtval.$jstraceid};
      }

      var traceid;
      if (rtval.$constrBy) {
        //see if this is a constructed object. if so, use the
        //constructor's name as the name of the type.
        var ctype = __typedJsTypes[rtval.$constrBy];
        traceid = ctype.type.namehint;
      }
      else {
        traceid = gensym("obj");
      }

      var typeObj = {};
      var isArray = false;
      var itemType = [];

      //mark the object
      try {
        rtval.$jstraceid = traceid;
        if (rtval.$jstraceid != traceid) 
          throw "Can't deal with this object.";
          
        if (rtval instanceof Array) {
          isArray = true;
          itemType = [];
          for (var i = 0; i < rtval.length; i++) {
            mergeRttypes(itemType, reffed_rttype(rtval[i]));
          }
        } 
        else {
          for (var p in rtval) {
            if (p == "$jstraceid") continue;
            if (p == "$constrBy") continue;
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
      }
      catch (errorr) {
        //various possibilities:
        //"security error" with them not wanting you to enumerate an object
        //inability to set $jstraceid on a native object..
        //so just give it type DOM! =)
        //we can't return an object here, because the traceid might not
        //have stuck on the object (that might have thrown the exception)
        return {kind: 'flat', type: "Unknown"};
      }

      var resRttype = {kind:'object', type:typeObj, seen: false,
                       constructed: rtval.$constrBy !== undefined,
                       constrName: (rtval.$constrBy ? traceid : undefined),
                       isArray: isArray, itemType: itemType};
      __typedJsTypes[traceid] = resRttype;
      return resRttype;
    }
    else if (res == "function") {
      if (!rtval.$jstraceid) {
        //untraced function = something we cant inspect
        return {kind: 'flat', type: "UntracedFunction"};
      }
      //handled with the tracing
      return {kind: 'function_ref', type: rtval.$jstraceid};
    }
    else if (res == "number") {
      if (Math.round(rtval) == rtval)
        return {kind:'flat', type:'Int'};
      else
        return {kind:'flat', type:'Double'};
    }
    else if (res == "string") {
      return {kind:'flat', type:'String'};
    }
    else if (res == "boolean") {
      return {kind:'flat', type:'Bool'};
    }
    else if (res == "undefined") {
      return {kind:'flat', type:'Void'};
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
    if (rt1===null && rt2===null) return true;
    if (rt1===null || rt2===null) return false;

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
          res += " + ";
        res += s;
        elementsIn += 1;
        needComma = true;
        segmentsSeen.push(s);
        copyFrom(innard.typesSeen, typesSeen);
      }
    }
    if (elementsIn != 1) res = "(" + res + ")";
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
  //if forceConstrs is true, then it will unravel object types even if they
  //were constructed. otherwise it just returns the type name
  var strType = function(t, dbg, forceConstrs) {
    incdbg();
    if (dbg) debug("strType " + _quickStrType(t));
    var typesSeen = {};
    if (t === undefined) {
      decdbg();
      return {answer: "Any", typesSeen: typesSeen}; //no info available
    }

    if (t.kind == "flat") {
      decdbg();
      return {answer: t.type, typesSeen: typesSeen};
    }

    if (t.kind == "object") {
      if (t.constructed && !forceConstrs) {
        decdbg();
        return {answer: t.constrName, typesSeen: typesSeen};
      }
      if (t.isArray) {
        var res = "Array<";
        if (t.itemType.length == 0) 
          res += "Any";
        else {
          var innard = strUnion(t.itemType, dbg);
          res += innard.answer;
          copyFrom(innard.typesSeen, typesSeen);
        }
        res += ">";
        decdbg();
        return {answer: res, typesSeen: typesSeen};
      }
      else {      
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
          res += p + " : " + innard.answer;
          copyFrom(innard.typesSeen, typesSeen);
        }
        decdbg();
        return {answer: res + "}", typesSeen: typesSeen};
      }
      
    }

    if (t.kind == "function") {
      var res = "(";
      if (t.type.thist && t.type.refsthis) {
        if (dbg) debug("function this type...");
        var innard = strUnion(t.type.thist, dbg);
        var strthist = innard.answer;
        if (!(strthist == "{}" || strthist == "Global"))
            res += "[" + strthist + "] ";
        copyFrom(innard.typesSeen, typesSeen);
      }

      if (t.type.args) {
        for (var i=0; i < t.type.args.length; i++) {
          if (dbg) debug("function arg #" + i + "...");
          var innard = strUnion(t.type.args[i], dbg);
          res += innard.answer;
          if (i != t.type.args.length-1)
            res += " * ";
          copyFrom(innard.typesSeen, typesSeen);
        }
      }
      else {
        //no info available, print as many Anys as named args
        for (var ii=0; ii<t.type.numNamedArgs; ii++) {
          res += "Any";
          if (ii != t.type.numNamedArgs - 1) res += " * ";
        }
      }

      res += " -> ";

      if (t.type.ret === undefined)
        res += "Any";
      else
      {
        if (dbg) debug("function ret type...");
        
        var innard;
        if (t.type.constrOrFunc === "constructor") {
          innard = strType(__typedJsTypes[t.type.namehint], dbg, true);
        }
        else {       
          innard = strUnion(t.type.ret, dbg);
        }
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
      var realObj = __typedJsTypes[t.type];
      if (realObj === undefined) {
        decdbg();
        return {answer: "BROKEN " + t.kind + "(" + t.type + ")", typesSeen: typesSeen};
      }

      typesSeen[t.type] = true;
      if (realObj.seen)
      {
        //we're in a "rec" construct
        decdbg();
        //return {answer: t.type, typesSeen: typesSeen};
        return {answer: "ERROR - RECURSIVE STRUCTURES ", typesSeen: typesSeen};
      }

      realObj.seen = true;
      var innard = strType(realObj, dbg)
      var strRealObj = innard.answer;

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
  //"foroutput" is true if just print without any names.
  var strNestedNamedRttype = function(nrt, dbg, foroutput) {
    var typesSeen = {};
    if (nrt === undefined) {
      return {answer: ["ERROR: BLANK"], typesSeen: typesSeen};
    }
    var n = nrt.name;
    if (!n) n = "";
    var rt = nrt.rttype;
    var line = "";

    if (rt.kind === "function_ref") {
      rt = __typedJsTypes[rt.type];
    }
    if (rt.kind === "function") {
      var corf = rt.type.constrOrFunc;
      if (foroutput) {
        if (corf === "constructor") line += corf + " ";
      }
      else {
        for (var nlev=0;nlev<rt.type.nestingLevel;nlev++) {
          line += "  ";
        }
        line += corf + " " + n + " : ";
      }
    }
    else if (n) {
      if (foroutput) {
      }
      else {
        line += n + " : ";
      }
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

    /*if (isFunc === false || isFunc.nested.length === 0) {
      return {answer: [line], typesSeen: typesSeen};
    }*/
    //cut off parens if it's for output
    if (foroutput) {
      if (line.charAt(0) == "(") {
        line = line.substring(1,line.length-1);
      }
    }
      
    var res = [line];

    /*for (var i=0; i < isFunc.nested.length; i++) {
      var innerRes = strNestedNamedRttype(isFunc.nested[i], dbg, foroutput);
      for (var j = 0; j < innerRes.answer.length; j++) {
        res.push((foroutput ? "" : "  ") + innerRes.answer[j]);
      }
      copyFrom(innerRes.typesSeen, typesSeen);
    }*/

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


  //code to update the tracing output:
  var update_tracewin = function() {
    clearwin(__dbgwin);
    clearwin(__tracewin);
    clearwin(__outwin);
    var sawSomething = false;

    var typesSeen = {};
    
    //with function names for testing:
    for (var fn in __orderedVars) {
      sawSomething = true;
      
      println(__tracewin, "// *** TYPES FOR " + fn + " *** ");
      println(__tracewin, "/*::");
      var ovs = __orderedVars[fn];
      
      if (ovs.length == 0) {
        println(__tracewin, "No tracing info yet...");
        continue;
      }

      for (var i=0; i < ovs.length; i++) {
        println(__dbgwin, "stringing var #" + i);
        var res = strNestedNamedRttype(ovs[i], true, false);
        println(__dbgwin, "done with var #" + i);
        var strs = res.answer;
        copyFrom(res.typesSeen, typesSeen);
        for (var j = 0; j < strs.length; j++) {
          println(__tracewin, "  " + strs[j]);
        }
      }
      println(__tracewin, "*/");
    }

    //output for the file
    typesSeen = {};
    for (var fn in __orderedVars) {
      sawSomething = true;
      
      println(__outwin, "// *** TYPES FOR " + fn + " *** ");
      var ovs = __orderedVars[fn];
      
      if (ovs.length == 0) {
        println(__outwin, "No tracing info yet...");
        continue;
      }

      for (var i=0; i < ovs.length; i++) {
        var res = strNestedNamedRttype(ovs[i], true, true);
        var strs = res.answer;
        copyFrom(res.typesSeen, typesSeen);
        for (var j = 0; j < strs.length; j++) {
          println(__outwin, strs[j]);
        }
      }
    }
    
    if (!sawSomething) {
      println(__tracewin, "No tracing info at all...");      
      println(__outwin, "No tracing info at all...");      
      return;
    }

    //if we're not in google gadgets, show results right away:
    if (!isGG) {
      showwin(__tracewin);
      showwin(__outwin);
      showwin(__dbgwin);
    }    
  };


  setInterval(update_tracewin, 1000);
  //setInterval(breathe, 1000);

  //wrapping functions:
  //-----------------------------------------------------
  //wrap every function with this function. it makes
  //every function call add trace information.
  //nestingLevel is how nested into other funcs this func is (just for output)
  //undefined if top-level. otherwise it should be wrapped
  //name is the name, if any (just for output)
  //filename is name of the file it appears in, to know where to put it
  //position is what position it occupies in the nester, e.g.
  //0 if the first position, 1 if the 2nd, etc.
  var tracefunction = function(fn, nestingLevel, name, filename, position) {
    var dbg = false;
    
    if (arguments.length != 5 || typeof nestingLevel != "number" || typeof position != "number") {
      alert("Please re-compile this code!");
      return;
    }

    //initialize the structs:
    var traceid = gensym("func" + (name ? ("_" + (name.substring(name.lastIndexOf(".")+1))) : ""));
    var func_rttype = {
      kind: "function",
      type: {
        numNamedArgs: fn.length,
        args: undefined,       
        ret: undefined,
        thist: undefined,
        nestingLevel: nestingLevel, //strictly for pretty output
        constrOrFunc: "unknown",
        namehint: name,
        refsthis: false}, //true if "this" is ever referenced inside this function
      seen: false};
    var tjstype = func_rttype.type;
    var ref = {
      kind: "function_ref",
      type: traceid};
    
    //however, see if we've already traced this position
    //if so, use those structs instead
    if ((filename in __orderedVars) && __orderedVars[filename][position]) {
      var ov = __orderedVars[filename][position];
      var prevname = ov.name;
      var prevref = ov.rttype;
      if (prevref && prevref.kind === "function_ref") {
        var prevtype = __typedJsTypes[prevref.type];
        if (prevtype.type.constrOrFunc === "shellfunc" ||
           prevname === name) {
          //we've already traced this position, just re-use it!
          traceid = prevref.type;
          func_rttype = prevtype;
          tjstype = prevtype.type;
          ref = {kind: "function_ref", type: traceid};
        }
      }
    }
        
    //insert it in the right place in the global, if it's not there
    if (!(filename in __orderedVars))
      __orderedVars[filename] = [];
    __orderedVars[filename][position] = {name: name, rttype: ref};

    var res = function() {
      //since all calls to 'new' are wrapped, $callingAsNew will be set
      //if it was called as a constructor
      //to catch some more cases, "this instanceof arguments.callee" also works sometimes
      var calledWithNew = res.$callingAsNew || this instanceof arguments.callee
      if (dbg) debug("traced function called!" + calledWithNew);

      //mark this function as either a function or a constructor
      if (calledWithNew) {
        if (tjstype.constrOrFunc == "function")
        {
          //alert("Can't deal with functions called both as constrs and funcs!");
          tjstype.constrOrFunc = "ERROR:FUNC+CONSTR";
        }
        else
          tjstype.constrOrFunc = "constructor";
      }
      else {
        if (tjstype.constrOrFunc == "constructor")
        {
          //alert("Can't deal with functions called both as constrs and funcs!");
          tjstype.constrOrFunc = "ERROR:FUNC+CONSTR";
        }
        else
          tjstype.constrOrFunc = "function";
      }

      //if we have a function, then the this type is valid upon
      //entering the function. if it's a constr, there is no 'this' type
      if (!calledWithNew) {
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
        //mark the object as being constructed by this function
        this.$constrBy = traceid;
        
        //update the type of the constructed element upon exiting the constructor
        rttype(this);
        /*tjstype.thist = mergeRttypes(
          tjstype.thist, reffed_rttype(this));*/
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

  //this fills out the orderedvars array with initial values, so if
  //a function never gets called, we can put in the right number
  //of Anys.
  var initnumargs = function(filename, argsarray) {
    if (!(filename in __orderedVars))
      __orderedVars[filename] = [];
    else
      alert("initnumargs called twice for no good reason");

    for (var i=0; i < argsarray.length; i++) {
      var traceid = gensym("shellfunc");
      var func_rttype = {
        kind: "function",
        type: {
          numNamedArgs: argsarray[i],
          args: undefined,       
          ret: undefined,
          thist: undefined,
          nestingLevel: 0, 
          constrOrFunc: "shellfunc",
          namehint: "",
          refsthis: false},
        seen: false};
      var tjstype = func_rttype.type;
      var ref = {
        kind: "function_ref",
        type: traceid};
  
      __orderedVars[filename][i] = {name: "SHILL", rttype: ref};    
      __typedJsTypes[traceid] = func_rttype;
    }
  }

  //wrap every call to "new" with this function
  //if the constructor has a $jstraceid, then the newly
  //created object will have a $constrby field to indicate
  //whence it came.
  //WARNING: ABUSE OF EVAL INCOMING
  var newwrapper = function (constr, args) {
    var argHolder = {"c": constr};
    for (var i=0; i < args.length; i++) {
      argHolder["$" + i] = args[i];
    }

    var newStr = "new (argHolder['c'])(";
    for (var i=0; i < args.length; i++) {
      newStr += "argHolder['$" + i + "']";
      if (i != args.length - 1) newStr += ", ";
    }
    newStr += ");";

    //if it's a non-wrapped constructor (e.g. Date), do nothing:
    if (!Object_hasOwnProperty(constr, "$jstraceid")) {
      return eval(newStr);
    }

    //otherwise let the tracing func know whats going on:
    if (!Object_hasOwnProperty(constr, "$callingAsNew"))
      constr.$callingAsNew = 0;

    constr.$callingAsNew += 1;
    var res = eval(newStr);
    constr.$callingAsNew -= 1;

    return res;
  }
  
  //wrap ever ref to "this"
  //it will alert func that its "this" type matters
  //this could be done statically but it'd involve
  //50 lines of boring case breakdowns 
  var thisref = function (_this, func) {
    if (func.$jstraceid !== undefined) {
      var t = __typedJsTypes[func.$jstraceid];
      t.type.refsthis = true;
    }
    return _this;
  };
  
  //this function will add our menu items to GG:
  //it must be called after the compiled code runs, in case they
  //add their own menu items.
  var addMenuItems = function (menu) {
      for (var i = 0; i < __windows.length; i++) {
        menu.addItem(
          __windows[i].title, 0, 
          (function (w) { return function(){showwin(w);} })(__windows[i]));
      }
    };    
  
  return {
    __typedjs: tracefunction,
    __new: newwrapper,
    __thisref: thisref,
    showTypes: function () { return showwin(__tracewin); },
    addMenuItems: addMenuItems,
    initnumargs: initnumargs
  };
})();

var __typedjs = holder.__typedjs;
var __new = holder.__new;
var showTypes = holder.showTypes;
var __thisref = holder.__thisref;
var __ADDMENUITEMS = holder.addMenuItems;
var __initnumargs = holder.initnumargs;