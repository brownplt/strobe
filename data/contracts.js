// The type of a contract combinator is: name -> args ... -> contract
//
// name is a human-readable name for the contract.  args .. are constructor-
// specified arguments and contract is the resulting contract.

var contracts = { };

contracts.map = function(f,arr) {
  var dest = [ ];
  for (var i = 0; i < arr.length; i++) {
    dest.push(f(arr[i]));
  }
  return dest;
};


contracts.zipWith = function(f,arr1,arr2) {
  var dest = [ ];
  var len = Math.min(arr1.length, arr2.length);
  for (var i = 0; i < len; i++) {
    dest.push(f(arr1[i],arr2[i]));
  }
  return dest;
};


contracts.blame = function(guilty,expected,received,message,loc) {
  var guiltyMsg = typeof(guilty) == "string"
                    ? guilty : guilty.value;
  var msg = "contract violation: expected " + expected + ", but received "
    + received + "\n" + loc;
  var err = new Error(msg);
  err.guilty = msg;
  err.blamed = guiltyMsg;
  err.expected = expected;
  err.received = received;
  err.guardLoc = loc;
  try {
    console.log(err);
  }
  catch(_) {

  };
  throw err;
};

contracts.flat = function(name) {
  return function(pred) {
    return {
      pred: pred,
      server: function(s,loc) {
        return function(val) {
          if (pred(val)) {
            return val;
            }
          else {
            contracts.blame(s,name,val,"does not satisfy the predicate",loc);
         }
        };
      },
      client: function(s,loc) {
        return function(val) { return val; };
      }
    };
  };
};

contracts.obj = function(name) {
  return function(fields) {
    return {
      pred: function(v) { return typeof v === "object"; },
      server: function(s,loc) {
        return function(val) {
          if (typeof val !== "object") {
            contracts.blame(s,name,val, "is not an object");
          }

          var ret = { };
          for (var f in fields) {
            if (fields[f] != fields.__proto__[f]) {
                ret[f] = fields[f].server(s,loc)(val[f]);
            }
          }
          return ret;
        };
      },
      client: function(s,loc) {
        return function(val) {
          if (typeof val !== "object") {
            return val;
          }

          var ret = { };
          for (var f in fields) {
            if (fields[f] != fields.__proto__[f]) {
                ret[f] = fields[f].client(s,loc)(val[f]);
            }
          }
          return ret;
        };
      }
    };
  };
};

contracts.union = function(name) {
  return function() {
    var ctcs = map(function(x) { return x; }, arguments);
    var ctc = undefined;
    var pickPred = function(v) {
        if (ctc != undefined) {
            return ctc;
        }

        for (var i = 0; i < ctcs.length; i++) {
            if (ctcs[i].pred(val)) {
                ctc = ctcs[i];
                return ctc;
            }
        };
        return false;
    };

    return {
      pred: function(v) { return pickPred(v) && pickPred(v).pred(v); },
      server: function(s,loc) {
        return function(val) {
            return pickPred(val).server(s,loc)(val);
        };
        },
      client: function(s,loc) {
        return function(val) {
            return pickPred(val).server(s,loc)(val);
        }
      }
    };
  };
};




contracts.varArityFunc = function(name) {
  return function(fixedArgs,restArgs,result) {
    return {
      pred: function(v) { return typeof v === "function"; },
      server: function(s,loc) {
        return function(proc) {
          if (typeof(proc) == "function") {
            return function() {
              var guardedArgs = contracts.zipWith(function(ctc,arg) {
                return ctc.client(s,loc)(arg);
              }, fixedArgs, arguments);
              for (var i = fixedArgs.length; i < arguments.length; i++) {
                guardedArgs.push(restArgs.client(s,loc)(arguments[i]));
              }
              return result.server(s,loc)(proc.apply(this, guardedArgs));
            };
          }
          else { contracts.blame(s,name, proc,"not a function",loc); }
        }
      },
      client: function(s,loc) {
        return function(proc) {
          if (typeof(proc) == "function") {
            return function() {
              var guardedArgs = contracts.zipWith(function(ctc,arg) {
                return ctc.server(s,loc)(arg);
              }, fixedArgs, arguments);
              for (var i = fixedArgs.length; i < arguments.length; i++) {
                guardedArgs.push(restArgs.server(s,loc)(arguments[i]));
              }
              return result.client(s,loc)(proc.apply(this, guardedArgs));
            };
          }
          else {
            return proc;
          }
        };
      }
    };
  };
};


contracts.Undefined = contracts.flat("Undef")(function(val) {
  return val === undefined;
});

contracts.NotUndef = contracts.flat("NotUndef")(function(val) {
  return val !== undefined;
});

contracts.Str = contracts.flat("Str")(function(v) {
    return typeof v === "string";
});

contracts.Int = contracts.flat("Int")(function(v) {
    return typeof v === "number" && (Math.round(v) == v);
});

contracts.Num = contracts.flat("Num")(function(v) {
    return typeof v === "number";
});

contracts.Bool = contracts.flat("Bool")(function(v) {
    return typeof v === "boolean";
});

contracts.Instanceof = function(klass) {
  return contracts.flat("instanceof")(function(v) {
    return v instanceof klass;
  });
};

contracts.Any = contracts.flat("Any")(function(v) {
  return true;
});

// pos is the name of val.  neg should be the name of the calling context.
// loc is the location where the contract is declared.
contracts.guard = function(ctc,val,pos,neg,loc) {
  return ctc.client(neg,loc)(ctc.server(pos,loc)(val));
};

/* End of contracts.js ********************************************************/
