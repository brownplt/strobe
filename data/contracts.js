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
  try { console.log(err); } catch(_) { };
  throw err;
}
 
contracts.flat = function(name) {
  return function(pred) {
    return {
      flat: function(val) { return pred(val); },
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

contracts.varArityFunc = function(name) {
  return function(fixedArgs,restArgs,result) {
    return {
      isHigherOrder: true,
      flat: function(val) { return typeof(val) == "function"; },
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


contracts.undefined = contracts.flat("undefined", function(val) { 
  return val === undefined;
});

contracts.String = function(v) {
    return typeof v === "string";
};

contracts.Int = function(v) {
    return typeof v === "number";
};


// pos is the name of val.  neg should be the name of the calling context.
contracts.guard = function(ctc,val,pos,neg,loc) {
  return ctc.client(neg,loc)(ctc.server(pos,loc)(val));
};

/* End of contracts.js ********************************************************/