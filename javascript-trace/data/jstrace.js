//__typedJsVars["\"flapjax.js\" (line 38, column 13)"]

var $jstraceid = "$global"; //mark the global object so we don't deeply inspect it

var __typedJsVars = {}; //map var names to their rttypes
var __typedJsTypes = {$global: {kind: 'object', type: {}}}; //map type aliases to their rttypes

//an rttype is an object of form:
//{kind: the kind of the type, one of: object_ref, function_ref, object, function, flat, undefined
// type: the type information}
//a union-rttype is an array of rttypes

//for flat, type is a string from typeof
//for object, it is an object mapping property names to rttypes
//for function, it is an object:
//  {args: an array of union-rttypes indicating the rttypes of the arguments
//   ret: the union-rttype of the ret type
///  thist: the union-rttype of the 'this' type}
//objects and functions also have a 'seen' field which is true if we've seen the object while
//  trying to find out its type (meaning we have a recursive type)

//for a object_ref or function_ref, it is a string representing the type alias. the type it
//  refers to can be found in __typedJsTypes

var __typedjs = (function() {  //lambda to hide all these local funcs
  var symnum = 0;
  var gensym = function(name) {
    if (name) {
      return "$" + name + (symnum++);
    }
    return "$gen" + (symnum++);
  }

  var rttype = function(rtval) { //pjs value --> rttype (as described above)
    var res = typeof(rtval);
    if (res == "object") {
      if ("$jstraceid" in rtval) {
        //pln("((SAWD IT REDY (" + rtval.$jstraceid + ":: " + strType(__typedJsTypes[rtval.$jstraceid]).answer + ")");
        return {kind: 'object_ref', type: rtval.$jstraceid};
      }
      
      typeObj = {};

      //mark the object
      rtval.$jstraceid = gensym("obj");
            
      for (var p in rtval) {
        if (p == "$jstraceid") continue;
        //pln("((SEEING PROPERTY: " + p + "))");
        try {
          var property = rtval[p];
        } catch (errorrrr) {
          //'security exception' with skype or something
          var property = undefined;
        }
        typeObj[p] = rttype(property);
      }
      
      var resRttype = {kind:'object', type:typeObj, seen: false};
      __typedJsTypes[rtval.$jstraceid] = resRttype;      
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
  }
  
  var reffed_rttype = function (rtval) {
    //return the rttype, and return a reference to a type if possible
    var rtt = rttype(rtval);
    if (rtt.kind == "object")
      return {kind: 'object_ref', type: rtval.$jstraceid};
    if (rtt.kind == "function") 
      return {kind: 'function_ref', type: rtval.$jstraceid};
    return rtt;
  }
  
  var rtequal = function(rt1, rt2) { //whether two rttypes are equal
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
  }

  var strUnion = function(u) { //converts an array of rttypes to a union
    if (u.length == 1) return strType(u[0]);
    
    var segmentsSeen = [];
    var needComma = false;
    var elementsIn = 0;
    var res = "";
    var typesSeen = {};
    
    for (var j=0; j < u.length; j++) {
      var innard = strType(u[j]);
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
    return {answer: res, typesSeen: typesSeen};
  };
  
  var strType = function(t) { 
    //input: rttype
    //output: object where 
    //{answer: the string result of the answer,
    // typesSeen: the names of the rttypes seen while iterating. needed to know whether
    //   to put a 'rec .' in}
    var typesSeen = {};
    if (t === undefined) return {answer: "any", typesSeen: typesSeen}; //no info available
    
    if (t.kind == "flat")
      return {answer: t.type, typesSeen: typesSeen};
      
    if (t.kind == "object") {
      var res = "{";
      var needComma = false;
      for (var p in t.type) {
        if (needComma) res += ", ";
        needComma = true;
        var innard = strType(t.type[p])
        res += p + " :: " + innard.answer;       
        copyFrom(innard.typesSeen, typesSeen);
      }
      return {answer: res + "}", typesSeen: typesSeen};
    }
    
    if (t.kind == "function") {
      var res = "(";
      if (t.type.thist) {
        var innard = strUnion(t.type.thist);
        var strthist = innard.answer;
        if (!(strthist == "{}"))
            res += "[" + strthist + "] ";
        copyFrom(innard.typesSeen, typesSeen); 
      }
      
      if (t.type.args) {
        for (var i=0; i < t.type.args.length; i++) {
          var innard = strUnion(t.type.args[i]);
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
        var innard = strUnion(t.type.ret);
        res += innard.answer;
        copyFrom(innard.typesSeen, typesSeen);
      }
      return {answer: res + ")", typesSeen: typesSeen};
    }
    
    if (t.kind == undefined)
      return {answer: "undefined", typesSeen: typesSeen};
      
    if (t.kind == "function_ref" || t.kind == "object_ref") {
      var realObj = __typedJsTypes[t.type];
      typesSeen[t.type] = true;
      if (realObj.seen)
      {
        //we're in a "rec" construct       
        return {answer: t.type, typesSeen: typesSeen};
      }
      
      realObj.seen = true;
      var innard = strType(realObj)
      var strRealObj = innard.answer;
      
      if (t.type in innard.typesSeen)
        var resStr = "rec " + t.type + " . " + strRealObj + "";
      else
        var resStr = strRealObj;
        
      realObj.seen = false;
      copyFrom(innard.typesSeen, typesSeen);
      return {answer: resStr, typesSeen: typesSeen};
    }
    
    return {answer: "UNKNOWN KIND: " + t.kind, typesSeen: typesSeen};
  };

  // Convert an array of arguments to an array of abstract arguments.
  var abstract = function(args) {
    var abstractArgs = [ ];
    for (var i = 0; i < args.length; i++) {
      abstractArgs.push(reffed_rttype(args[i]));
    }
    return abstractArgs;
  }
  
  //merge an array of possible rttypes with a new rttype.
  var mergeRttypes = function(existing, newone) {
    if (existing === undefined)
      return [newone];
    
    if (arrayContains(existing, newone))
      return existing;
    existing.push(newone);
    return existing;
  }

  // Merge two abstract argument arrays
  // existing is an array of unions of rttypes, args is an array of rttypes
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
  }
  
  //wrap every RHS with this function
  /*var tracevar = function(label, v) {
    var traceid = gensym("var");*/
      
  
  
  //wrap every function with this function.
  //WARNING: cannot always tell when we are called as a constructor
  var tracefunction = function(label, fn) {    
    var traceid = gensym("func");
    var func_rttype = {
      kind: "function",
      type: {
        args: undefined,
        ret: undefined,
        thist: undefined},
      seen: false};    
    var tjstype = func_rttype.type;

    var res = function() {
      var calledWithNew = this instanceof arguments.callee;
      
      if (!calledWithNew) {
        //update the type of this upon entering the function
        tjstype.thist = mergeRttypes(
          tjstype.thist, reffed_rttype(this)); 
      }
      
      tjstype.args = processArguments(
        tjstype.args, abstract(arguments));
        
      var r = fn.apply(this, arguments);

      tjstype.ret = mergeRttypes(
        tjstype.ret, reffed_rttype(r));

      if (calledWithNew) {
        //update the type of this upon exiting the constructor
        tjstype.thist = mergeRttypes(
          tjstype.thist, reffed_rttype(this)); 
      }
      return r;
    };
    res.$jstraceid = traceid;
    res.$label = label;

    __typedJsVars[label] = {
      kind: "function_ref",
      type: res.$jstraceid};
      
    __typedJsTypes[traceid] = func_rttype;

    return res;
  }

  //initialization code to open the popup and make the tracing code run
  var update_tracewin = function() {
  while (__resdiv.hasChildNodes()) 
      __resdiv.removeChild(__resdiv.firstChild);
      
    if (__typedJsVars.length == 0) {
      var txt = __tracewin.document.createTextNode("No tracing info yet...");
      __resdiv.appendChild(txt);
      __resdiv.appendChild(__tracewin.document.createElement('br'));
      return;
    }    
    
    for (var p in __typedJsVars) {
      var st = strType(__typedJsVars[p]).answer;
      var txt = __tracewin.document.createTextNode(p + " :: " + st);
      __resdiv.appendChild(txt);
      __resdiv.appendChild(__tracewin.document.createElement('br'));
    }
  }
  
  var __tracewin, __resdiv;
  
  __tracewin = window.open("", 
    document.title + " traced types", "width=640,height=480,scrollbars=yes");
      
  //for firefox: 'zero' is doctype, '1' is the htmlelement
  var twbody = __tracewin.document.childNodes.item(1).childNodes.item(1);
  while (twbody.hasChildNodes()) twbody.removeChild(twbody.firstChild);
  
  __resdiv = __tracewin.document.createElement("div");
  __resdiv.innerHTML = "Tracing initialized! Results come soon";
    
  twbody.appendChild(__resdiv);
  
  setInterval(update_tracewin, 500);
  
  return tracefunction;
})();
  

  
  
  
  
  