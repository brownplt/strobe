/*
 * Copyright (c) 2006-2009, The Flapjax Team.  All Rights Reserved.
 *  
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * * Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * * Neither the name of the Brown University, the Flapjax Team, nor the names
 *   of its contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 */
  
///////////////////////////////////////////////////////////////////////////////
// Miscellaneous functions

var module = this;

//credit 4umi
//slice: Array a * Integer * Integer -> Array a
var slice = function (arr, start, stop) {
	var i, len = arr.length, r = [];
	if( !stop ) { stop = len; }
	if( stop < 0 ) { stop = len + stop; }
	if( start < 0 ) { start = len - start; }
	if( stop < start ) { i = start; start = stop; stop = i; }
	for( i = 0; i < stop - start; i++ ) { r[i] = arr[start+i]; }
	return r;
}

var isEqual = function (a,b) {
	return (a == b) ||
		( (((typeof(a) == 'number') && isNaN(a)) || a == 'NaN') &&
		  (((typeof(b) == 'number') && isNaN(b)) || b == 'NaN') );
};

var forEach = function(fn,arr) {
  for (var i = 0 ; i < arr.length; i++) {
    fn(arr[i]);
  }
};

//member: a * Array b -> Boolean
var member = function(elt, lst) {
	for (var i = 0; i < lst.length; i++) { 
		if (isEqual(lst[i], elt)) {return true;} 
	}
	return false;
};

var zip = function(arrays) {
	if (arrays.length == 0) return [];
	var ret = [];
	for(var i=0; i<arrays[0].length;i++) {
		ret.push([]);
		for(var j=0; j<arrays.length;j++) 
			ret[i].push(arrays[j][i]);
	}
	return ret;
}


var map1 = function(f,src) { 
  var dest = [ ];
  for (var i = 0; i < src.length; i++) { dest.push(f(src[i])); }
  return dest;
};


//map: (a * ... -> z) * [a] * ... -> [z]
var map = function (fn) {
	var arrays = slice(arguments, 1);
	if (arrays.length === 0) { return []; }
	else if (arrays.length === 1) {
		var ret = [];
		for(var i=0; i<arrays[0].length; i++) {ret.push(fn(arrays[0][i]));}
		return ret;
	}
	else {
		var ret = zip(arrays);
		var o = new Object();
		for(var i=0; i<ret.length; i++) {ret[i] = fn.apply(o,ret[i]);}
		return ret;
	}
};
  
//filter: (a -> Boolean) * Array a -> Array a
var filter = function (predFn, arr) {
	var res = [];
	for (var i = 0; i < arr.length; i++) { 
		if (predFn(arr[i])) { res.push(arr[i]); }
	}
	return res;
};
  
//fold: (a * .... * accum -> accum) * accum * [a] * ... -> accum
//fold over list(s), left to right
var fold = function(fn, init /* arrays */) {
	var lists = slice(arguments, 2);
	if (lists.length === 0) { return init; }
	else if(lists.length === 1) {
		var acc = init;
		for(var i = 0; i < lists[0].length; i++) {
			acc = fn(lists[0][i],acc);
		}
		return acc;
	}
	else {
		var acc = init;
		for (var i = 0; i < lists[0].length; i++) {
			var args = map( function (lst) { return lst[i];}, 
	          lists);
			args.push(acc);
			acc = fn.apply({}, args);
		}
		return acc;
	}
};
  
//foldR: (a * .... * accum -> accum) * accum * [a] * ... -> accum
//fold over list(s), right to left, fold more memory efficient (left to right)
var foldR = function (fn, init /* arrays */) {
  var lists = slice(arguments, 2);
	if (lists.length === 0) { return init; }
	else if(lists.length === 1) {
		var acc = init;
		for(var i=lists[0].length - 1; i > -1; i--)
			acc = fn(lists[0][i],acc);
		return acc;
	}
	else {
		var acc = init;
		for (var i = lists[0].length - 1; i > -1; i--) {
			var args = map( function (lst) { return lst[i];}, 
	          lists);
			args.push(acc);
			acc = fn.apply({}, args);
		}
		return acc;     
	}
};

//////////////////////////////////////////////////////////////////////////////
// Flapjax core

// Sentinel value returned by updaters to stop propagation.
var doNotPropagate = { };

// 


//Pulse: Stamp * Path * Obj
var Pulse = function (stamp, value) {
  // Timestamps are used by liftB (and ifE).  Since liftB may receive multiple
  // update signals in the same run of the evaluator, it only propagates the 
  // signal if it has a new stamp.
  this.stamp = stamp;
  this.value = value;
};


//Probably can optimize as we expect increasing insert runs etc
var PQ = function () {
  var ctx = this;
  ctx.val = [];
  this.insert = function (kv) {
		ctx.val.push(kv);
		var kvpos = ctx.val.length-1;
		while(kvpos > 0 && kv.k < ctx.val[Math.floor((kvpos-1)/2)].k) {
			var oldpos = kvpos;
			kvpos = Math.floor((kvpos-1)/2);
			ctx.val[oldpos] = ctx.val[kvpos];
			ctx.val[kvpos] = kv;
		}
  };
  this.isEmpty = function () { 
    return ctx.val.length === 0; 
  };

  this.peek = function() {
    return ctx.val[0];
  };
    
  this.pop = function () {
		if(ctx.val.length == 1) {
			return ctx.val.pop();
		}
		var ret = ctx.val.shift();
		ctx.val.unshift(ctx.val.pop());
		var kvpos = 0;
		var kv = ctx.val[0];
		while(1) { 
			var leftChild = (kvpos*2+1 < ctx.val.length ? ctx.val[kvpos*2+1].k : kv.k+1);
			var rightChild = (kvpos*2+2 < ctx.val.length ? ctx.val[kvpos*2+2].k : kv.k+1);
			if(leftChild > kv.k && rightChild > kv.k)
				  break;

			if(leftChild < rightChild) {
				ctx.val[kvpos] = ctx.val[kvpos*2+1];
				ctx.val[kvpos*2+1] = kv;
				kvpos = kvpos*2+1;
			}
			else {
				ctx.val[kvpos] = ctx.val[kvpos*2+2];
				ctx.val[kvpos*2+2] = kv;
				kvpos = kvpos*2+2;
			}
		}
		return ret;
	};
};

var lastRank = 0;
var stamp = 1;
var nextStamp = function () { return ++stamp; };

// A queue keyed by real time.
var timeQueue = new PQ();

var currentlyPropagating = false;

var propagate = function(now) {
	// If when == now,  
	if (currentlyPropagating) {
		// If when <= now, pulseQueue will be reached by the existing call to
		// propagatePulse
		return;
	}

	currentlyPropagating = true;
	try {
		var pulseQueue = timeQueue.peek();
    if (pulseQueue && pulseQueue.k <= now) {
			pulseQueue = timeQueue.pop().pulses;
	
			var nextPulse, i;
			var len = 1;
			while (len) {
				var qv = pulseQueue.pop();
				len--;
				nextPulse = qv.n.updater(new Pulse(qv.v.stamp, qv.v.value));
				if (nextPulse != doNotPropagate) {
					for (i = 0; i < qv.n.sendsTo.length; i++) {
						len++;
						pulseQueue.insert({ k:qv.n.sendsTo[i].rank,
																n:qv.n.sendsTo[i],
																v:nextPulse });
					}
				}
			}
	
			pulseQueue = timeQueue.peek();
         
		}
      
	}
	finally {
		currentlyPropagating = false;
	}

}

var propagatePulse = function(pulse, node, when) {
  var now = (new Date()).getTime();

  // Create a new queue of pulses that starts with the incoming pulse.
  var pulseQueue = new PQ();
  pulseQueue.insert({ k: node.rank, n: node, v: pulse });

  // Schedule the new pulse queue, possible in the future.
  when = when ? (now + when) : now;  
  timeQueue.insert({ k: when,  pulses: pulseQueue });

  propagate(now);
} 


//Event: Array Node b * ( (Pulse a -> Void) * Pulse b -> Void)
var EventStream = function (nodes,updater) {
  this.updater = updater;
  
  this.sendsTo = []; //forward link
  
  for (var i = 0; i < nodes.length; i++) {
    nodes[i].sendsTo.push(this);
  }
  
  this.rank = ++lastRank;
};
EventStream.prototype = new Object();

//createNode: Array Node a * ( (Pulse b ->) * (Pulse a) -> Void) -> Node b
var createNode = function (nodes, updater) {
	return new EventStream(nodes,updater);
};

//attachListener: Node * Node -> Void
//flow from node to dependent
//note: does not add flow as counting for rank nor updates parent ranks
var attachListener = function (node, dependent) {
  if (!(node instanceof EventStream)) { throw 'attachListenenerNode: expects event as first arg';} //SAFETY
  if (!(dependent instanceof EventStream)) { throw 'attachListenenerNode: expects event as second arg';} //SAFETY
  
  node.sendsTo.push(dependent);
	if(node.rank > dependent.rank) {
		var lowest = lastRank+1;
		var q = [dependent];
		while(q.length) {
			var cur = q.splice(0,1)[0];
			cur.rank = ++lastRank;
			q = q.concat(cur.sendsTo);
		}
	}
};
module.attachListener = attachListener;

//removeListener: Node * Node -> Boolean
//remove flow from node to dependent
//note: does not remove flow as counting for rank nor updates parent ranks
var removeListener = function (node, dependent)
{
  if (!(node instanceof EventStream)) { throw 'removeListenerNode: expects event as first arg';} //SAFETY
  if (!(dependent instanceof EventStream)) { throw 'removeListenenerNode: expects event as second arg';} //SAFETY
  
  var foundSending = false;
  for (var i = 0; i < node.sendsTo.length && !foundSending; i++) {
    if (node.sendsTo[i] == dependent) {
      node.sendsTo.splice(i, 1);
      foundSending = true;
    }
  }
  
  return foundSending;
};

module.removeListener = removeListener;

// An internalE is a node that simply propagates all pulses it receives.  It's used internally by various 
// combinators.
var internalE = function(dependsOn) {
  return createNode(dependsOn || [ ],function(pulse) { return pulse; });
}

var zeroE = function() {
  return createNode([],function(pulse) {
      throw ('zeroE : received a value; zeroE should not receive a value; the value was ' + pulse.value);
  });
};


var oneE = function(val) {
  var sent = false;
  var evt = createNode([],function(pulse) {
    if (sent) {
      throw ('oneE : received an extra value');
    }
    sent = true;
    return pulse;
  });
  window.setTimeout(function() { sendEvent(evt,val); },0);
  return evt;
};


// a.k.a. mplus; mergeE(e1,e2) == mergeE(e2,e1)
var mergeE = function() {
  if (arguments.length == 0) {
    return zeroE();
  }
  else {
    var deps = slice(arguments,0);
    return internalE(deps);
  }
};


EventStream.prototype.mergeE = function() {
  var deps = slice(arguments,0);
  deps.push(this);
  return internalE(deps);
};


EventStream.prototype.constantE = function(constantValue) {
  return createNode([this],function(pulse) {
    pulse.value = constantValue;
    return pulse;
  });
};


var constantE = function(e,v) { return e.constantE(v); };


//This is up here so we can add things to its prototype that are in flapjax.combinators
var Behavior = function (event, init, updater) {
  if (!(event instanceof EventStream)) { 
    throw 'Behavior: expected event as second arg'; 
  }
  
  var behave = this;
  this.last = init;
  
  //sendEvent to this might impact other nodes that depend on this event
  //sendBehavior defaults to this one
  this.underlyingRaw = event;
  
  //unexposed, sendEvent to this will only impact dependents of this behaviour
  this.underlying = createNode([event], updater 
    ? function (p) {
        behave.last = updater(p.value); 
        p.value = behave.last; return p;
      } 
    : function (p) {
        behave.last = p.value;
        return p
      });
};
Behavior.prototype = new Object();



var receiverE = function() {
  var evt = internalE();
  evt.sendEvent = function(value) {
    propagatePulse(new Pulse(nextStamp(), value),evt);
  };
  return evt;
};


//note that this creates a new timestamp and new event queue
var sendEvent = function (node, value, when) {
  if (!(node instanceof EventStream)) { throw 'sendEvent: expected Event as first arg'; } //SAFETY
  
  propagatePulse(new Pulse(nextStamp(), value), node, when);
};

// bindE :: EventStream a * (a -> EventStream b) -> EventStream b
EventStream.prototype.bindE = function(k) {
  /* m.sendsTo resultE
   * resultE.sendsTo prevE
   * prevE.sendsTo returnE
   */
  var m = this;
  var prevE = false;
  
  var outE = createNode([],function(pulse) { return pulse; });
  outE.name = "bind outE";
  
  var inE = createNode([m], function (pulse) {
    if (prevE) { 
      removeListener(prevE,outE);
    }
    prevE = k(pulse.value);
    if (prevE instanceof EventStream) {
      attachListener(prevE,outE);
    }
    else {
      throw "bindE : expected EventStream";
    }

    return doNotPropagate;
  });
  inE.name = "bind inE";
  
  return outE;
};

/* Could be written as:
 *
 * e.bindE(function(v) { return oneE(f(v)); })
 */
EventStream.prototype.mapE = function(f) {
  if (!(f instanceof Function)) {
    throw ('mapE : expected a function as the first argument; received ' + f);
  };
  
  return createNode([this],function(pulse) {
    pulse.value = f(pulse.value);
    return pulse;
  });
};


EventStream.prototype.notE = function() { return this.mapE(function(v) { return !v; }); };


var notE = function(e) { return e.notE(); };


EventStream.prototype.filterE = function(pred) {
  if (!(pred instanceof Function)) {
    throw ('filterE : expected predicate; received ' + pred);
  };
  
  // Can be a bindE
  return createNode([this], function(pulse) {
    return pred(pulse.value) ? pulse : doNotPropagate;
  });
};


var filterE = function(e,p) { return e.filterE(p); };


// Fires just once.
EventStream.prototype.onceE = function() {
  var done = false;
  // Alternately: this.collectE(0,\n v -> (n+1,v)).filterE(\(n,v) -> n == 1).mapE(fst)
  return createNode([this],function(pulse) {
    if (!done) { done = true; return pulse; }
    else { return doNotPropagate; }
  });
};


var onceE = function(e) { return e.onceE(); };


EventStream.prototype.skipFirstE = function() {
  var skipped = false;
  return createNode([this],function(pulse) {
    if (skipped)
      { return pulse; }
    else
      { return doNotPropagate; }
  });
};


var skipFirstE = function(e) { return e.skipFirstE(); };


EventStream.prototype.collectE = function(init,fold) {
  var acc = init;
  return this.mapE(
    function (n) {
      var next = fold(n, acc);
      acc = next;
      return next;
    });
};


var collectE = function(e,i,f) { return e.collectE(i,f); };


// a.k.a. join
EventStream.prototype.switchE = function() {
  return this.bindE(function(v) { return v; });
};


var switchE = function(e) { return e.switchE(); };


EventStream.prototype.ifE = function(thenE,elseE) {
  var testStamp = -1;
  var testValue = false;
  
  createNode([this],function(pulse) { testStamp = pulse.stamp; testValue = pulse.value; return doNotPropagate; });
  
  return mergeE(createNode([thenE],function(pulse) { if (testValue && (testStamp == pulse.stamp)) { send(pulse); } }),
    createNode([elseE],function(pulse) { if (!testValue && (testStamp == pulse.stamp)) { send(pulse); } }));
};


var ifE = function(test,thenE,elseE) {
  if (test instanceof EventStream)
    { return test.ifE(thenE,elseE); }
  else
    { return test ? thenE : elseE; }
};

    
var andE = function (/* . nodes */) {
  var nodes = slice(arguments, 0);
  
  var acc = (nodes.length > 0)? 
  nodes[nodes.length - 1] : oneE(true);
  
  for (var i = nodes.length - 2; i > -1; i--) {
    acc = ifE(
      nodes[i], 
      acc, 
      nodes[i].constantE(false));
  }
  return acc;
};


EventStream.prototype.andE = function( /* others */ ) {
  var deps = [this].concat(slice(arguments,0));
  return andE.apply(this,deps);
};


var orE = function () {
  var nodes = slice(arguments, 0);
  var acc = (nodes.length > 2)? 
  nodes[nodes.length - 1] : oneE(false); 
  for (var i = nodes.length - 2; i > -1; i--) {
    acc = ifE(
      nodes[i],
      nodes[i],
      acc);
  }
  return acc;
};


EventStream.prototype.orE = function(/*others*/) {
  var deps = [this].concat(slice(arguments,0));
  return orE.apply(this,deps);
};


var delayStaticE = function (event, time) {
  updateResolution(time);
  var resE = internalE();
  
  createNode([event], function (p) { 
    sendEvent(resE, p.value, time);
    return doNotPropagate;
  });
  
  return resE;
};

//delayE: Event a * [Behavior] Number ->  Event a
EventStream.prototype.delayE = function (time) {
  var event = this;
  
  if (time instanceof Behavior) {
    
    var receiverEE = internalE();
    var link = 
    {
      from: event, 
      towards: delayStaticE(event, valueNow(time))
    };
    
		//TODO: Change semantics such that we are always guaranteed to get an event going out?
    var switcherE = 
    createNode(
      [changes(time)],
      function (p) {
        removeListener(link.from, link.towards); 
        link =
        {
          from: event, 
          towards: delayStaticE(event, p.value)
        };
        sendEvent(receiverEE, link.towards);
        return doNotPropagate;
      });
    
    var resE = receiverEE.switchE();
    
    sendEvent(switcherE, valueNow(time));
    return resE;
    
      } else { return delayStaticE(event, time); }
};


var delayE = function(sourceE,interval) {
  return sourceE.delayE(interval);
};


//mapE: ([Event] (. Array a -> b)) . Array [Event] a -> [Event] b
var mapE = function (fn /*, [node0 | val0], ...*/) {
  //      if (!(fn instanceof Function)) { throw 'mapE: expected fn as second arg'; } //SAFETY
  
  var valsOrNodes = slice(arguments, 0);
  //selectors[i]() returns either the node or real val, optimize real vals
  var selectors = [];
  var selectI = 0;
  var nodes = [];
  for (var i = 0; i < valsOrNodes.length; i++) {
    if (valsOrNodes[i] instanceof EventStream) {
      nodes.push(valsOrNodes[i]);
      selectors.push( 
        (function(ii) {
            return function(realArgs) { 
              return realArgs[ii];
            };
        })(selectI));
      selectI++;
    } else {
      selectors.push( 
        (function(aa) { 
            return function () {
              return aa;
            }; 
        })(valsOrNodes[i]));
    } 
  }
  
  var context = this;
	var nofnodes = slice(selectors,1);
  
  if (nodes.length === 0) {
    return oneE(fn.apply(context, valsOrNodes));
  } else if ((nodes.length === 1) && (fn instanceof Function)) {
    return nodes[0].mapE(
      function () {
        var args = arguments;
        return fn.apply(
          context, 
          map(function (s) {return s(args);}, nofnodes));
      });
  } else if (nodes.length === 1) {
    return fn.mapE(
      function (v) {
        var args = arguments;
        return v.apply(
          context, 
          map(function (s) {return s(args);}, nofnodes));
      });                
  } else if (fn instanceof Function) {
    return createTimeSyncNode(nodes).mapE(
      function (arr) {
        return fn.apply(
          this,
          map(function (s) { return s(arr); }, nofnodes));
      });
  } else if (fn instanceof EventStream) {
    return createTimeSyncNode(nodes).mapE(
      function (arr) {
        return arr[0].apply(
          this, 
          map(function (s) {return s(arr); }, nofnodes));
      });
      } else {throw 'unknown mapE case';}
};


EventStream.prototype.snapshotE = function (valueB) {
  return createNode([this], function (pulse) {
    pulse.value = valueNow(valueB);
    return pulse;
  });
};


var snapshotE = function(triggerE,valueB) {
  return triggerE.snapshotE(valueB);
};


EventStream.prototype.filterRepeatsE = function(optStart) {
  var hadFirst = optStart === undefined ? false : true;
  var prev = optStart;

  return this.filterE(function (v) {
    if (!hadFirst || !(isEqual(prev,v))) {
      hadFirst = true;
      prev = v;
      return true;
    }
    else {
      return false;
    }
  });
};


var filterRepeatsE = function(sourceE,optStart) {
  return sourceE.filterRepeatsE(optStart);
};


//credit Pete Hopkins
var calmStaticE = function (triggerE, time) {
	var out = internalE();
  createNode(
    [triggerE],
    function() {
      var towards = null;
      return function (p) {
        if (towards !== null) { clearTimeout(towards); }
        towards = setTimeout( function () { towards = null; sendEvent(out,p.value) }, time );
        return doNotPropagate;
      };
    }());
	return out;
};

//calmE: Event a * [Behavior] Number -> Event a
EventStream.prototype.calmE = function(time) {
  if (time instanceof Behavior) {
		var out = internalE();
    createNode(
      [this],
      function() {
        var towards = null;
        return function (p) {
          if (towards !== null) { clearTimeout(towards); }
          towards = setTimeout( function () { towards = null; sendEvent(out,p.value) }, valueNow(time));
          return doNotPropagate;
        };
      }());
		return out;
  } else {
    return calmStaticE(this,time);       
  }
};


var calmE = function(sourceE,interval) {
  return sourceE.calmE(interval);
};


EventStream.prototype.blindE = function (time) {
  return createNode(
    [this],
    function () {
      var intervalFn = 
      time instanceof Behavior?
      function () { return valueNow(time); }
      : function () { return time; };
      var lastSent = (new Date()).getTime() - intervalFn() - 1;
      return function (p) {
        var curTime = (new Date()).getTime();
        if (curTime - lastSent > intervalFn()) {
          lastSent = curTime;
          return p;
        }
        else { return doNotPropagate; }
      };
    }());
};


var blindE = function(sourceE,interval) {
  return sourceE.blindE(interval);
};


EventStream.prototype.startsWith = function(init) {
  return new Behavior(this,init);
};


var startsWith = function(e,init) {
  if (!(e instanceof EventStream)) {
    throw 'startsWith: expected EventStream; received ' + e;
  }
  return e.startsWith(init); 
};


Behavior.prototype.valueNow = function() {
  return this.last;
};
var valueNow = function(behavior) { return behavior.valueNow(); };


Behavior.prototype.changes = function() {
  return this.underlying;
};


var changes = function (behave) { return behave.changes(); }


Behavior.prototype.switchB = function() {
  var behaviourCreatorsB = this;
  var init = valueNow(behaviourCreatorsB);
  
  var prevSourceE = null;
  
  var receiverE = new internalE();
  
	//XXX could result in out-of-order propagation! Fix!
  var makerE = 
  createNode(
    [changes(behaviourCreatorsB)],
    function (p) {
      if (!(p.value instanceof Behavior)) { throw 'switchB: expected Behavior as value of Behavior of first argument'; } //SAFETY
      if (prevSourceE != null) {
        removeListener(prevSourceE, receiverE);
      }
      
      prevSourceE = changes(p.value);
      attachListener(prevSourceE, receiverE);
      
      sendEvent(receiverE, valueNow(p.value));
      return doNotPropagate;
    });
  
  if (init instanceof Behavior) {
    sendEvent(makerE, init);
  }
  
  return startsWith(
    receiverE,
    init instanceof Behavior? valueNow(init) : init);
};


var switchB = function (b) { return b.switchB(); };


//TODO test, signature
var timerB = function(interval) {
  return startsWith(timerE(interval), (new Date()).getTime());
};


//TODO test, signature
var delayStaticB = function (triggerB, time, init) {
  return startsWith(delayStaticE(changes(triggerB), time), init);
};

//TODO test, signature
Behavior.prototype.delayB = function (time, init) {
  var triggerB = this;
  if (time instanceof Behavior) {
    return startsWith(
      delayE(
        changes(triggerB), 
        time),
      arguments.length > 3 ? init : valueNow(triggerB));
  } else {
    return delayStaticB(
      triggerB, 
      time,
      arguments.length > 3 ? init : valueNow(triggerB));
  }
};


var delayB = function(srcB, timeB, init) { 
  return srcB.delayB(timeB,init); 
};


//artificially send a pulse to underlying event node of a behaviour
//note: in use, might want to use a receiver node as a proxy or an identity map
Behavior.prototype.sendBehavior = function(val) {
  sendEvent(this.underlyingRaw,val);
};
Behavior.prototype.sendBehavior = Behavior.prototype.sendBehavior;

var sendBehavior = function (b,v) { b.sendBehavior(v); };



Behavior.prototype.ifB = function(trueB,falseB) {
  var testB = this;
  //TODO auto conversion for behaviour funcs
  if (!(trueB instanceof Behavior)) { trueB = constantB(trueB); }
  if (!(falseB instanceof Behavior)) { falseB = constantB(falseB); }
  return liftB(function(te,t,f) { return te ? t : f; },testB,trueB,falseB);
};


var ifB = function(test,cons,altr) {
  if (!(test instanceof Behavior)) { test = constantB(test); };
  
  return test.ifB(cons,altr);
};



//condB: . [Behavior boolean, Behavior a] -> Behavior a
var condB = function (/* . pairs */ ) {
  var pairs = slice(arguments, 0);
return liftB.apply({},[function() {
		for(var i=0;i<pairs.length;i++) {
			if(arguments[i]) return arguments[pairs.length+i];
		}
		return undefined;
	}].concat(map(function(pair) {return pair[0];},pairs).concat(map(function(pair) {return pair[1];},pairs))));
};


//TODO optionally append to objects
//createConstantB: a -> Behavior a
var constantB = function (val) {
  return new Behavior(internalE(), val);
};


var liftB = function (fn /* . behaves */) {

  var args = slice(arguments, 1);
  
  //dependencies
  var constituentsE =
    map(changes,
    filter(function (v) { return v instanceof Behavior; },
      arguments));
  
  //calculate new vals
  var getCur = function (v) {
    return v instanceof Behavior ? v.last : v;
  };
  
  var ctx = this;
  var getRes = function () {
    return getCur(fn).apply(ctx, map(getCur, args));
  };

  if(constituentsE.length == 1) {
    return new Behavior(constituentsE[0],getRes(),getRes);
  }
    
  //gen/send vals @ appropriate time
  var prevStamp = -1;
  var mid = createNode(constituentsE, function (p) {
    if (p.stamp != prevStamp) {
      prevStamp = p.stamp;
      return p; 
    }
    else {
      return doNotPropagate;
    }
  });
  
  return new Behavior(mid,getRes(),getRes);
};


Behavior.prototype.liftB = function(/* args */) {
  var args= slice(arguments,0).concat([this]);
  return liftB.apply(this,args);
};


var andB = function (/* . behaves */) {
return liftB.apply({},[function() {
		for(var i=0; i<arguments.length; i++) {if(!arguments[i]) return false;}
		return true;
}].concat(slice(arguments,0)));
};


Behavior.prototype.andB = function() {
  return andB([this].concat(arguments));
};


var orB = function (/* . behaves */ ) {
return liftB.apply({},[function() {
		for(var i=0; i<arguments.length; i++) {if(arguments[i]) return true;}
		return false;
}].concat(slice(arguments,0)));
};


Behavior.prototype.orB = function () {
  return orB([this].concat(arguments));
};


Behavior.prototype.notB = function() {
  return this.liftB(function(v) { return !v; });
};


var notB = function(b) { return b.notB(); };


Behavior.prototype.blindB = function (intervalB) {
  return changes(this).blindE(intervalB).startsWith(this.valueNow());
};


var blindB = function(srcB,intervalB) {
  return srcB.blindB(intervalB);
};


Behavior.prototype.calmB = function (intervalB) {
  return this.changes().calmE(intervalB).startsWith(this.valueNow());
};


var calmB = function (srcB,intervalB) { 
  return srcB.calmB(intervalB);
};

  
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// DOM Utilities

//credit Scott Andrew
//usage: addEvent(myDomObj, "mouseover", event->void )
//warning: do not use 'this' as meaning depends on browser (myDomObj vs window)
//addEvent: Dom * String DomEventEnum * (DomEvent -> a) -> Void
var addEvent = function (obj, evType, fn) {
	//TODO encode mouseleave evt, formalize new evt interface
  
	if (obj.addEventListener) {
    obj.addEventListener(evType, fn, false); //TODO true fails on Opera
    return true;
	} else if (obj.attachEvent) {
    //some reason original had code bloat here
    return obj.attachEvent("on"+evType, fn); 
	} else {
    return false; 
	}
};


// The Flapjax library does not use this function  
//credit Dustin Diaz 
//note: node/tag optional
//getElementsByClass: Regexp CSSSelector * Dom * String DomNodeEnum -> Array Dom
var getElementsByClass = function (searchClass, node, tag) {
	var classElements = [];
	if ( (node === null) || (node === undefined) ) { node = document; }
	if ( (tag === null) || (tag === undefined) ) { tag = '*'; }
	var els = node.getElementsByTagName(tag);
	var elsLen = els.length;
	var pattern = new RegExp("(^|\\s)"+searchClass+"(\\s|$)");
	for (var i = 0, j = 0; i < elsLen; i++) {
    if ( pattern.test(els[i].className) ) {
      classElements.push(els[i]);
    }
	}
	return classElements;
};


//assumes IDs already preserved
//swapDom: (Dom a U String) [* (Dom b U String)] -> Dom a
var swapDom = function(replaceMe, withMe) {
	if ((replaceMe === null) || (replaceMe === undefined)) { throw ('swapDom: expected dom node or id, received: ' + replaceMe); } //SAFETY
  
	var replaceMeD = getObj(replaceMe);
	if (!(replaceMeD.nodeType > 0)) { throw ('swapDom expected a Dom node as first arg, received ' + replaceMeD); } //SAFETY
  
	if (withMe) {
    var withMeD = getObj(withMe);
    if (!(withMeD.nodeType > 0)) { throw 'swapDom: can only swap with a DOM object'; } //SAFETY
    try {
      if (replaceMeD.parentNode == null) { return withMeD; }
      if(withMeD != replaceMeD) replaceMeD.parentNode.replaceChild(withMeD, replaceMeD);
    } catch (e) {
      throw('swapDom error in replace call: withMeD: ' + withMeD + ', replaceMe Parent: ' + replaceMeD + ', ' + e + ', parent: ' + replaceMeD.parentNode);                    
    }
	} else {
    replaceMeD.parentNode.removeChild(replaceMeD); //TODO isolate child and set innerHTML to "" to avoid psuedo-leaks?
	}
	return replaceMeD;
};


//getObj: String U Dom -> Dom
//throws 
//  'getObj: expects a Dom obj or Dom id as first arg'
//  'getObj: flapjax: cannot access object'
//  'getObj: no obj to get
//also known as '$'
//TODO Maybe alternative?
var getObj = function (name) {
	if (typeof(name) == 'object') { return name; }
	else if ((typeof(name) == 'null') || (typeof(name) == 'undefined')) {
    throw 'getObj: expects a Dom obj or Dom id as first arg';
	} else {
    
    var res = 
		document.getElementById ? document.getElementById(name) :
		document.all ? document.all[name] :
		document.layers ? document.layers[name] :
		(function(){ throw 'getObj: flapjax: cannot access object';})();
    if ((res === null) || (res === undefined)) { 
      throw ('getObj: no obj to get: ' + name); 
    }
    return res;
	}
};

var $ = getObj;



//helper to reduce obj look ups
//getDynObj: domNode . Array (id) -> domObj
//obj * [] ->  obj
//obj * ['position'] ->  obj
//obj * ['style', 'color'] ->  obj.style
var getMostDom = function (domObj, indices) {
  var acc = getObj(domObj);
  if ( (indices === null) || (indices === undefined) || (indices.length < 1)) {
    return acc;
  } else {
    for (var i = 0; i < indices.length - 1; i++) {
      acc = acc[indices[i]];
    }
    return acc;
  }       
};

var getDomVal = function (domObj, indices) {
  var val = getMostDom(domObj, indices);
  if (indices && indices.length > 0) {
    val = val[indices[indices.length - 1]];
  }
  return val;
};

//TODO: manual timer management stinks.
// TODO: Name turn off or somethin
var ___timerID = 0;
var __getTimerId = function () { return ++___timerID; };    
var timerDisablers = [];

var disableTimerNode = function (node) { timerDisablers[node.__timerId](); };

var disableTimer = function (v) {
  if (v instanceof Behavior) { 
    disableTimerNode(v.underlyingRaw); 
  } else if (v instanceof EventStream) {
    disableTimerNode(v);
  }
};

var currentSchedulerResolution = 500; // fairly arbitrary
var schedulerID;

var delayedScheduler = function() {
  propagate((new Date()).getTime());
};

var updateResolution = function(newResolution) {
  if (newResolution < currentSchedulerResolution) {
    clearInterval(schedulerID);
    currentSchedulerResolution = newResolution;
    schedulerID = setInterval(delayedScheduler, newResolution);
  }
};


schedulerID = setInterval(delayedScheduler, currentSchedulerResolution);


var createTimerNodeStatic = function(interval) {
  updateResolution(interval);

  var disabled = false;

  var node = createNode([], function(pulse) { 
    // Send ourself an event in the future.
    if (!disabled) { 
      sendEvent(node, (new Date ()).getTime() + interval, interval);
      return pulse;
    }
    else {
      return doNotPropagate;
    }
  });

  var localSchedulerID = setInterval(delayedScheduler, interval);

  node.__timerId = __getTimerId();
  timerDisablers[node.__timerId] = function() {
    clearInterval(localSchedulerID);
    disabled = true;
  };

  sendEvent(node, (new Date ()).getTime(), interval);

  return node;
};
  


var timerE = function (interval) {
  if (interval instanceof Behavior) {
    var receiverE = internalE();
    
    //the return
    var res = receiverE.switchE();
    
    //keep track of previous timer to disable it
    var prevE = createTimerNodeStatic(valueNow(interval));
    
    //init
    sendEvent(receiverE, prevE);
    
    //interval changes: collect old timer
    createNode(
      [changes(interval)],
      function (p) {
        disableTimerNode(prevE); 
        prevE = createTimerNodeStatic(p.value);
        sendEvent(receiverE, prevE);
        return doNotPropagate;
      });
    
    res.__timerId = __getTimerId();
    timerDisablers[res.__timerId] = function () {
      disableTimerNode[prevE]();
      return;
    };
    
    return res;
  } else {
    return createTimerNodeStatic(interval);
  }
};


// Applies f to each element of a nested array.
var deepEach = function(arr, f) {
  for (var i = 0; i < arr.length; i++) {
    if (arr[i] instanceof Array) {
      deepEach(arr[i], f);
    }
    else {
      f(arr[i]);
    }
  }
};


var mapWithKeys = function(obj, f) {
  for (var ix in obj) {
    if (!(Object.prototype && Object.prototype[ix] == obj[ix])) {
      f(ix, obj[ix]);
    }
  }
}


var insertAfter = function(parent, newChild, refChild) {
  if (refChild.nextSibling) {
    parent.insertBefore(newChild, refChild.nextSibling);
  }
  else {
    // refChild == parent.lastChild
    parent.appendChild(newChild);
  }
};


var swapChildren = function(parent, existingChildren, newChildren) {
  var end = Math.min(existingChildren.length, newChildren.length);
  var i;

  for (i = 0; i < end; i++) {
    parent.replaceChild(newChildren[i], existingChildren[i]);
  }

  var lastInsertedChild = existingChildren[i - 1];

  if (end < existingChildren.length) {
    for (i = end; i < existingChildren.length; i++) {
      parent.removeChild(existingChildren[i]);
    }
  }
  else if (end < newChildren.length) {
    for (i = end; i < newChildren.length; i++) {
      insertAfter(parent, newChildren[i], newChildren[i - 1]);
    }
  }
};


var elementize /* not a word */ = function(maybeElement) {
  return (maybeElement.nodeType > 0) 
           ? maybeElement
           : document.createTextNode(maybeElement.toString());
};


var staticEnstyle = function(obj, prop, val) {
  if (val instanceof Object) {
    mapWithKeys(val, function(k, v) { enstyle(obj[prop], k, v); });
  }
  else {
    obj[prop] = val;
  }
};


var dynamicEnstyle = function(obj, prop, val) {
  if (val instanceof Behavior) {
    staticEnstyle(obj, prop, val.valueNow());
    val.liftB(function(v) {
      staticEnstyle(obj, prop, v);
    });
  }
  else if (val instanceof Object) {
    mapWithKeys(val, function(k, v) {
      dynamicEnstyle(obj[prop], k, v);
    });
  }
  else {
    obj[prop] = val;
  }
};
  

// makeTagB :: tagName -> elementB, ... -> element
var makeTagB = function(tagName) { return function() {
  var attribs, children;

  if (typeof(arguments[0]) == "object" && 
      !(arguments[0].nodeType > 0 || arguments[0] instanceof Behavior || 
        arguments[0] instanceof Array)) {
    attribs = arguments[0];
    children = slice(arguments, 1);
  }
  else {
    attribs = { };
    children = slice(arguments, 0);
  }
 
  var elt = document.createElement(tagName);

  mapWithKeys(attribs, function(name, val) {
    if (val instanceof Behavior) {
      elt[name] = val.valueNow();
      val.liftB(function(v) { 
        staticEnstyle(elt, name, v); });
    }
    else {
      dynamicEnstyle(elt, name, val);
    }
  });

  deepEach(children, function(child) {
    if (child instanceof Behavior) {
      var lastVal = child.valueNow();
      if (lastVal instanceof Array) {
        lastVal = map(elementize, lastVal);
        forEach(function(dynChild) { elt.appendChild(dynChild); }, lastVal);
        child.liftB(function(currentVal) {
          currentVal = map(elementize, currentVal);
          swapChildren(elt, lastVal, currentVal);
          lastVal = currentVal;
        });
      }
      else {
        lastVal = elementize(lastVal);
        elt.appendChild(lastVal);
        child.liftB(function(currentVal) {
          currentVal = elementize(currentVal);
          elt.replaceChild(currentVal, lastVal);
          lastVal = currentVal;
        });
      }
    }
    else {
      elt.appendChild(elementize(child));
    }
  });

  return elt;
} };




var generatedTags = 
[ "a", "b", "blockquote", "br", "button", "canvas", "div", "fieldset", 
"form", "font", "h1", "h2", "h3", "h4", "hr", "img", "iframe", "input", 
"label", "legend", "li", "ol", "optgroup", "option", 
"p", "pre", "select", "span", "strong", "table", "tbody", 
"td", "textarea", "tfoot", "th", "thead", "tr", "tt", "ul" ];

forEach(function(tagName) {
  this[tagName.toUpperCase()] = makeTagB(tagName);
}, generatedTags); // generatedTags is defined in flapjax-impl.js


//TEXTB: Behavior a -> Behavior Dom TextNode    
TEXTB = function (strB) {
  //      if (!(strB instanceof Behavior || typeof(strB) == 'string')) { throw 'TEXTB: expected Behavior as second arg'; } //SAFETY
  if (!(strB instanceof Behavior)) { strB = constantB(strB); }
  
  return startsWith(
    changes(strB).mapE(
      function (txt) { return document.createTextNode(txt); }),
    document.createTextNode(valueNow(strB)));
};

var TEXT = function (str) {
  return document.createTextNode(str);
};

///////////////////////////////////////////////////////////////////////////////
// Reactive DOM

//tagRec: Array (EventName a) * 
//      ( .Array (Event a) * Array (Event a) -> Behavior Dom) -> Behavior Dom

// tagRec :: [EventName] * (EventStream DOMEvent, ... -> Element) -> Element
var tagRec = function (eventNames, maker) {
  if (!(eventNames instanceof Array)) { throw 'tagRec: expected array of event names as first arg'; } //SAFETY
  if (!(maker instanceof Function)) { throw 'tagRec: expected function as second arg'; } //SAFETY
  
  var numEvents = eventNames.length;

  var receivers = [ ];
  var i;
  for (i = 0; i < numEvents; i++) {
    receivers.push(internalE());
  }

  var elt = maker.apply(this, receivers);

  for (i = 0; i < numEvents; i++) {
    attachListener(extractEventE(elt, eventNames[i]), receivers[i]);
  }

  return elt;
};


//extractEventStaticE: Dom * String -> Event
var extractEventStaticE = function (domObj, eventName) {
  if (!eventName) { throw 'extractEventE : no event name specified'; }
  if (!domObj) { throw 'extractEventE : no DOM element specified'; }
  
  domObj = getObj(domObj);
  
  var primEventE = internalE();
  addEvent(domObj,eventName,function(evt) {
     sendEvent(primEventE, evt || window.event);
     // Important for IE; false would prevent things like a checkbox actually
     // checking.
     return true;
  });
  
  return primEventE;
};

//extractEventE: [Behavior] Dom * String -> Event
var extractEventE = function (domB, eventName) {
  if (!(domB instanceof Behavior)) {
    return extractEventStaticE(domB,eventName);
  }
  else {
    var domE = domB.changes();
    
    var eventEE = domE.mapE(function(dom) {
      return extractEventStaticE(dom,eventName);
    });
    
    var resultE = eventEE.switchE();
    
    sendEvent(domE,domB.valueNow());
    
    return resultE;
  };
};

var $E = extractEventE;


//extractEventsE: 
//      [Behavior] Dom  
//      . Array String
//      -> Event
// ex: extractEventsE(m, 'body', 'mouseover', 'mouseout')
extractEventsE = function (domObj /* . eventNames */) {
  var eventNames = slice(arguments, 1);
  
  var events = map(
    function (eventName) {
      return extractEventE(domObj, eventName); 
    },
    eventNames.length === 0 ? [] : eventNames);
  
  return mergeE.apply(this, events);
};

//value of dom form object during trigger
extractValueOnEventE = function (triggerE, domObj) {
  if (!(triggerE instanceof EventStream)) { throw 'extractValueOnEventE: expected Event as first arg'; } //SAFETY
  
  return changes(extractValueOnEventB.apply(this, arguments));
  
};

//extractDomFieldOnEventE: Event * Dom U String . Array String -> Event a
extractDomFieldOnEventE = function (triggerE, domObj /* . indices */) {
  if (!(triggerE instanceof EventStream)) { throw 'extractDomFieldOnEventE: expected Event as first arg'; } //SAFETY
  var indices = slice(arguments, 2);
  var res =
  triggerE.mapE(
    function () { return getDomVal(domObj, indices); });
  return res;
};

var extractValueE = function (domObj) {
  return changes(extractValueB.apply(this, arguments));
};

//extractValueOnEventB: Event * DOM -> Behavior
// value of a dom form object, polled during trigger
var extractValueOnEventB = function (triggerE, domObj) {
  return extractValueStaticB(domObj, triggerE);
};

//extractValueStaticB: DOM [ * Event ] -> Behavior a
//If no trigger for extraction is specified, guess one
extractValueStaticB = function (domObj, triggerE) {
  
  var objD;
  try {
    objD = getObj(domObj);
    //This is for IE
    if(typeof(domObj) == 'string' && objD.id != domObj) {
    	throw 'Make a radio group';
    }
  } catch (e) {
    objD = {type: 'radio-group', name: domObj};
  }
  
  var getter; // get value at any current point in time
  
  
  switch (objD.type)  {
    
    //TODO: checkbox.value instead of status?
  case 'checkbox': 
    
    return startsWith(
      filterRepeatsE(
        extractDomFieldOnEventE(
          triggerE ? triggerE : 
          extractEventsE(
            objD, 
            'click', 'keyup', 'change'),
          objD,
          'checked'),objD.checked),
      objD.checked);
    
    case 'select-one':
      
      getter = function (_) {                         
        return objD.selectedIndex > -1 ? 
        (objD.options[objD.selectedIndex].value ?
          objD.options[objD.selectedIndex].value :
          objD.options[objD.selectedIndex].innerText)
		    : undefined;
      };
      
      return startsWith(
        filterRepeatsE(
            (triggerE ? triggerE :
            extractEventsE(
              objD,
              'click', 'keyup', 'change')).mapE(getter)),getter(),
        getter());
      
      case 'select-multiple':
        //TODO ryan's cfilter adapted for equality check
        
        getter = function (_) {
          var res = [];
          for (var i = 0; i < objD.options.length; i++) {
            if (objD.options[i].selected) {
              res.push(objD.options[i].value ? objD.options[i].value : objD.options[i].innerText);
            }
          }
          return res;
        };
        
        
        return startsWith(
            (triggerE ? triggerE : 
            extractEventsE(
              objD,
              'click', 'keyup', 'change')).mapE(getter),
          getter());
        
        case 'text':
        case 'textarea':
        case 'hidden':
        case 'password':
          
          return startsWith(
            filterRepeatsE(
              extractDomFieldOnEventE(
                triggerE ? triggerE :
                extractEventsE(
                  objD, 
                  'click', 'keyup', 'change'),
                objD,
                'value'),objD.value),
            objD.value);
          
          case 'button': //same as above, but don't filter repeats
            
            return startsWith(
              extractDomFieldOnEventE(
                triggerE ? triggerE :
                extractEventsE(
                  objD, 
                  'click', 'keyup', 'change'),
                objD,
                'value'),
              objD.value);
            
            
            case 'radio': 
            case 'radio-group':
              
              //TODO returns value of selected button, but if none specified,
              //      returns 'on', which is ambiguous. could return index,
              //      but that is probably more annoying
              
              var radiosAD = filter(
                function (elt) { 
                  return (elt.type == 'radio') &&
                  (elt.getAttribute('name') == objD.name); 
                },
                document.getElementsByTagName('input'));
              
              getter = 
              objD.type == 'radio' ?
              
              function (_) {
                return objD.checked;
              } :
              
              function (_) {
                for (var i = 0; i < radiosAD.length; i++) {
                  if (radiosAD[i].checked) {
                    return radiosAD[i].value; 
                  }
                }
                return undefined; //TODO throw exn? 
              };
              
              var actualTriggerE = triggerE ? triggerE :
              mergeE.apply(
                this,
                map(
                  function (radio) { 
                    return extractEventsE(
                      radio, 
                  'click', 'keyup', 'change'); },
                    radiosAD));
              
              return startsWith(
                filterRepeatsE(
                    actualTriggerE.mapE(getter),getter()),
                getter());
              
              default:
                
                throw ('extractValueStaticB: unknown value type "' + objD.type + '"');
  }
};

var extractValueB = function (domObj) {
  if (domObj instanceof Behavior) {
    return liftB(function (dom) { return extractValueStaticB(dom); },
                  domObj)
           .switchB();
  } else {
    return extractValueStaticB(domObj);
  }
};
var $B = extractValueB;


//into[index] = deepValueNow(from) via descending from object and mutating each field
deepStaticUpdate = function (into, from, index) {
  var fV = (from instanceof Behavior)? valueNow(from) : from;
  if (typeof(fV) == 'object') {
    for (var i in fV) {
      if (!(Object.prototype) || !(Object.prototype[i])) {
        deepStaticUpdate(index? into[index] : into, fV[i], i);
      }
    }
  } else {
    var old = into[index];
    into[index] = fV;
  }
};

//note: no object may be time varying, just the fields
//into[index] = from
//only updates on changes
deepDynamicUpdate = function (into, from, index) {
  var fV = (from instanceof Behavior)? valueNow(from) : from;
  if (typeof(fV) == 'object') {
    if (from instanceof Behavior) {
      throw 'deepDynamicUpdate: dynamic collections not supported';
    }
    for (var i in fV) {
      if (!(Object.prototype) || !(Object.prototype[i])) {
        deepDynamicUpdate(index? into[index] : into, fV[i], i);
      }
    }
  } else {
    if (from instanceof Behavior) {
      createNode(
        [changes(from)],
        function (p) {
          if (index) { 
            var old = into[index];
            into[index] = p.value;
          }
          else { into = p.value; } //TODO notify topE?
          return doNotPropagate;
        });
    }
  }
};


insertValue = function (val, domObj /* . indices */) {
  var indices = slice(arguments, 2);
  var parent = getMostDom(domObj, indices);
  deepStaticUpdate(parent, val, indices ? indices[indices.length - 1] : undefined);    	
};

//TODO convenience method (default to firstChild nodeValue) 
insertValueE = function (triggerE, domObj /* . indices */) {
  if (!(triggerE instanceof EventStream)) { throw 'insertValueE: expected Event as first arg'; } //SAFETY
  
  var indices = slice(arguments, 2);
  var parent = getMostDom(domObj, indices);
  
    triggerE.mapE(function (v) {
      deepStaticUpdate(parent, v, indices? indices[indices.length - 1] : undefined);
    });
};

//insertValueB: Behavior * domeNode . Array (id) -> void
//TODO notify adapter of initial state change?
insertValueB = function (triggerB, domObj /* . indices */) { 
  
  var indices = slice(arguments, 2);
  var parent = getMostDom(domObj, indices);
  
  
  //NOW
  deepStaticUpdate(parent, triggerB, indices ? indices[indices.length - 1] : undefined);
  
  //LATER
  deepDynamicUpdate(parent, triggerB, indices? indices[indices.length -1] : undefined);
  
};

//TODO copy dom event call backs of original to new? i don't thinks so
//  complication though: registration of call backs should be scoped
insertDomE = function (triggerE, domObj) {
  
  if (!(triggerE instanceof EventStream)) { throw 'insertDomE: expected Event as first arg'; } //SAFETY
  
  var objD = getObj(domObj);
  
  var res = triggerE.mapE(
    function (newObj) {
      //TODO safer check
      if (!((typeof(newObj) == 'object') && (newObj.nodeType == 1))) { 
        newObj = module.SPAN({}, newObj);
      }
      swapDom(objD, newObj);
      objD = newObj;
      return newObj; // newObj;
    });
  
  return res;
};

//insertDom: dom 
//          * dom 
//          [* (null | undefined | 'over' | 'before' | 'after' | 'leftMost' | 'rightMost' | 'beginning' | 'end']
//          -> void
// TODO: for consistency, switch replaceWithD, hookD argument order
insertDomInternal = function (hookD, replaceWithD, optPosition) {
  switch (optPosition)
  {
  case undefined:
  case null:
  case 'over':
    swapDom(hookD,replaceWithD);
    break;
  case 'before':  
    hookD.parentNode.insertBefore(replaceWithD, hookD);
    break;
  case 'after':
    if (hookD.nextSibling) {
      hookD.parentNode.insertBefore(replaceWithD, hookD.nextSibling);
    } else {
      hookD.parentNode.appendChild(replaceWithD);
    }
    break;
  case 'leftMost':
    if (hookD.parentNode.firstChild) { 
      hookD.parentNode.insertBefore(
        replaceWithD, 
        hookD.parentNode.firstChild);
              } else { hookD.parentNode.appendChild(replaceWithD); }
              break;
            case 'rightMost':
              hookD.parentNode.appendChild(replaceWithD);
              break;
            case 'beginning':
              if (hookD.firstChild) { 
                hookD.insertBefore(
                  replaceWithD, 
                  hookD.firstChild);
              } else { hookD.appendChild(replaceWithD); }
              break;
            case 'end':
              hookD.appendChild(replaceWithD);
              break;
            default:
              throw ('domInsert: unknown position: ' + optPosition);
  }
};

//insertDom: dom 
//          * dom U String domID 
//          [* (null | undefined | 'over' | 'before' | 'after' | 'leftMost' | 'rightMost' | 'beginning' | 'end']
//          -> void
insertDom = function (replaceWithD, hook, optPosition) {
  //TODO span of textnode instead of textnode?
  insertDomInternal(
    getObj(hook), 
    ((typeof(replaceWithD) == 'object') && (replaceWithD.nodeType > 0)) ? replaceWithD :
    document.createTextNode(replaceWithD),	    
    optPosition);     	    
};

//TODO test
//insertDomB: 
//      [Behavior] String U Dom 
//      [* ( id U null U undefined ) 
//          [* ('before' U 'after' U 'leftMost' U 'rightMost' U 'over' U 'beginning' U 'end')]]
//      -> Behavior a
//if optID not specified, id must be set in init val of trigger
//if position is not specified, default to 'over'
//performs initial swap onload    
insertDomB = function (initTriggerB, optID, optPosition) {
  
  if (!(initTriggerB instanceof Behavior)) { 
    initTriggerB = constantB(initTriggerB);
  }
  
  var triggerB = 
  liftB(
    function (d) { 
      if ((typeof(d) == 'object') && (d.nodeType >  0)) {
        return d;
      } else {
        var res = document.createElement('span'); //TODO createText instead
        res.appendChild(document.createTextNode(d));
        return res;
      }
    },
    initTriggerB);
  
  var initD = valueNow(triggerB);
  if (!((typeof(initD) == 'object') && (initD.nodeType == 1))) { throw ('insertDomB: initial value conversion failed: ' + initD); } //SAFETY	
  
  insertDomInternal(
    optID === null || optID === undefined ? getObj(initD.getAttribute('id')) : getObj(optID), 
    initD, 
    optPosition);
  
  var resB = startsWith(
    insertDomE(
      changes(triggerB),
      initD), 
    initD);
  
  return resB;
};


var extractIdB = function (id, start)
{
  return startsWith(
    createNode( start instanceof Behavior? [changes(start)] :
      [],
      function (p) {
        p.value = getObj(id);
        return p;
      }),
    getObj(id));
};

var mouseE = function(elem) {
  return extractEventE(elem,'mousemove')
  .mapE(function(evt) {
      if (evt.pageX | evt.pageY) {
        return { left: evt.pageX, top: evt.pageY };
      }
      else if (evt.clientX || evt.clientY) {
        return { left : evt.clientX + document.body.scrollLeft,
                 top: evt.clientY + document.body.scrollTop };
      }
      else {
        return { left: 0, top: 0 };
      }
  });
};

var mouseB = function(elem) {
  return mouseE(elem).startsWith({ left: 0, top: 0 });
}


var mouseLeftB = function(elem) {
  return liftB(function(v) { return v.left; },mouseB(elem));
};


var mouseTopB = function(elem) {
  return mouseB(elem).liftB(function(v) { return v.top; });
};



var clicksE = function(elem) {
  return extractEventE(elem,'click');
};


//////////////////////////////////////////////////////////////////////////////
// Combinators for web services


//credit Matt White
var getURLParam = function (param) {
  var lparam = param.toLowerCase();
  var aReturn = [];
  var strHref = window.location.href;
  var endstr = (strHref.indexOf('#') > -1) ? strHref.indexOf('#') : strHref.length;
  if ( strHref.indexOf("?") > -1 ) {
    var strQueryString = strHref.substring(strHref.indexOf("?")+1,endstr);
    map(function(qp) {
        var eq = qp.indexOf('=');
        var qname = qp.substr(0,eq+1).toLowerCase();
        if(qname == lparam+"=") aReturn.push(decodeURIComponent(qp.substr(eq+1)));
    },strQueryString.split("&"));
  }
  if (aReturn.length == 0) return undefined;
  else if(aReturn.length == 1) return aReturn[0];
  else return aReturn;
};


//credit Quirksmode
//readCookie: String -> String U Undefined
var readCookie = function (name) {
  var nameEQ = name + "=";
  var ca = document.cookie.split(';');
  for (var i=0; i < ca.length; i++) {
    var co = ca[i];
    while (co.charAt(0) == ' ') { co = co.substring(1, co.length); }
    if (co.indexOf(nameEQ) === 0) { 
      return co.substring(nameEQ.length, co.length);
    }
  }
  return undefined;       
};


//========== dynamic scripts ==========
var scriptCounter = 0;
var deleteScript = function (scriptID) {
  var scriptD = getObj(scriptID);
  scriptD.parentNode.removeChild(scriptD); //TODO isolate child and set innerHTML to "" to avoid psuedo-leaks?
};

// optional fn/param that gets polled until parm is defined
var runScript = function (url, fn, param) {
  var script = document.createElement("script");
  script.src = url;
  var scriptID = 'scriptFnRPC' + scriptCounter++;
  script.setAttribute('id', scriptID);
  document.getElementsByTagName("head").item(0).appendChild(script);
  var timer = {};
  var check = 
  function () {
    eval("try { if (" + param + "!== undefined) {var stat = " + param + ";}} catch (e) {}");
    if (stat !== undefined) {
      eval(param + " = undefined;");
      clearInterval(timer.timer);
      clearInterval(timer.timeout);
      if (fn instanceof Function) {
        fn(stat); 
      }
      deleteScript(scriptID);
    }
  };
  timer.timer = setInterval(check, 3500);
  timer.timeout = 
  setTimeout( 
    function () { 
      try { clearInterval(timer.timer); }
      catch (e) {}
    },
    5000); //TODO make parameter?
};

// Node {url, globalArg} -> Node a
//load script @ url and poll until param is set, then pass it along
var evalForeignScriptValE = function(urlArgE) {
  var result = receiverE();
  urlArgE.mapE(function(urlArg) {
      runScript(urlArg.url,
        function(val) { result.sendEvent(val); }, 
        urlArg.globalArg);
  });
  
  return result;
};


var ajaxRequest = function(method,url,body,async,useFlash,callback) {
  var xhr;
  if (useFlash) {
    xhr = new FlashXMLHttpRequest();
    xhr.onload = function() { callback(xhr); };
  }
  else if (window.XMLHttpRequest && !(window.ActiveXObject)) {
    xhr = new window.XMLHttpRequest();
    xhr.onload = function() { callback(xhr); };
  }
  else if (window.ActiveXObject) {
    try { xhr = new ActiveXObject("Msxml2.XMLHTTP"); }
    catch(e) { xhr = new ActiveXObject("Microsoft.XMLHTTP"); }
    
    xhr.onreadystatechange = function() {
      if (xhr.readyState == 4) { callback(xhr); }
    };
  };
  
  xhr.open(method,url,async);
  
  if (method == 'POST') {
    xhr.setRequestHeader('Content-Type','application/x-www-form-urlencoded');
  }
  
  xhr.send(body);
  return xhr;
};

var encodeREST = function(obj) {
  var str = "";
  for (var field in obj) {
    if (typeof(obj[field]) !== 'function') { // skips functions in the object
      if (str != '') { str += '&'; }
      str += field + '=' + encodeURIComponent(obj[field]);
    }
  }
  return str;
};

// From json.org
var parseJSON = function(str) {
  try {
      return !(/[^,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t]/.test(
              str.replace(/"(\\.|[^"\\])*"/g, ''))) &&
          eval('(' + str + ')');
  } catch (e) {
      throw 'cannot parse JSON string: ' + e;
  }
};

var toJSONString = (function() {
  var m = {
          '\b': '\\b',
          '\t': '\\t',
          '\n': '\\n',
          '\f': '\\f',
          '\r': '\\r',
          '"' : '\\"',
          '\\': '\\\\'
      };
  var s = {
    array: function (x) {
      var a = ['['], b, f, i, l = x.length, v;
      for (i = 0; i < l; i += 1) {
        v = x[i];
        f = s[typeof v];
        if (f) {
          v = f(v);
          if (typeof v == 'string') {
            if (b) {
              a[a.length] = ',';
            }
            a[a.length] = v;
            b = true;
          }
        }
      }
      a[a.length] = ']';
      return a.join('');
    },
    'boolean': function (x) {
      return String(x);
    },
    'null': function (x) {
      return "null";
    },
    number: function (x) {
      return isFinite(x) ? String(x) : 'null';
    },
    object: function (x) {
      if (x) {
        if (x instanceof Array) {
          return s.array(x);
        }
        var a = ['{'], b, f, i, v;
        for (i in x) {
          v = x[i];
          f = s[typeof v];
          if (f) {
            v = f(v);
            if (typeof v == 'string') {
              if (b) {
                a[a.length] = ',';
              }
              a.push(s.string(i), ':', v);
              b = true;
            }
          }
        }
        a[a.length] = '}';
        return a.join('');
      }
      return 'null';
    },
    string: function (x) {
      if (/["\\\x00-\x1f]/.test(x)) {
        x = x.replace(/([\x00-\x1f\\"])/g, function(a, b) {
            var c = m[b];
            if (c) {
              return c;
            }
            c = b.charCodeAt();
            return '\\u00' +
            Math.floor(c / 16).toString(16) +
            (c % 16).toString(16);
        });
      }
      return '"' + x + '"';
    }
  };
  return function(val) {
    var f = s[typeof val];
    if (f) { return f(val); }
    else { throw 'parseJSON: unknown object type: ' + (typeof val); }
  };
})();
 
var serverRequestE = function(useFlash,requestE) {
  var responseE = receiverE();
  
  requestE.mapE(function (obj) {
      var body = '';
      var method = 'GET';
      var url = obj.url;
      
      var reqType = obj.request ? obj.request : (obj.fields ? 'post' : 'get');
      if (obj.request == 'get') {
        url += "?" + encodeREST(obj.fields);
        body = '';
        method = 'GET';
      } else if (obj.request == 'post') {
        body = toJSONString(obj.fields); 
        method = 'POST';
      } else if (obj.request == 'rawPost') {
        body = obj.body;
        method = 'POST';
      }
      else if (obj.request == 'rest') {
        body = encodeREST(obj.fields);
        method = 'POST';
      }
      else {
        throw("Invalid request type: " + obj.request);
      }
      
      var async = obj.async;
      
      var xhr;
      
      // Branch on the response type to determine how to parse it
      if (obj.response == 'json') {
        xhr = ajaxRequest(method,url,body,async,useFlash,
          function(xhr) {
            responseE.sendEvent(parseJSON(xhr.responseText)); 
          });
      }
      else if (obj.response == 'xml') {
        ajaxRequest(method,url,body,async,useFlash,
          function(xhr) {
            responseE.sendEvent(xhr.responseXML);
          });
      }
      else if (obj.response == 'plain' || !obj.response) {
        ajaxRequest(method,url,body,async,useFlash,
          function(xhr) {
            responseE.sendEvent(xhr.responseText);
        });
      }
      else {
        throw('Unknown response format: ' + obj.response);
      }
  });
  
  return responseE;
};

var getWebServiceObjectE = function(requestE) {
  return serverRequestE(false,requestE);
};


var getForeignWebServiceObjectE = function(requestE) {
  return serverRequestE(true,requestE);
};


var cumulativeOffset = function(element) {
  var valueT = 0, valueL = 0;
  do {
    valueT += element.offsetTop  || 0;
    valueL += element.offsetLeft || 0;
    element = element.offsetParent;
  } while (element);
  return { left: valueL, top: valueT };
};

///////////////////////////////////////////////////////////////////////////////
// Flapjax compiler support


/* What is lifting?  Lifting a type-directed transformation that is applied to
 * functions that do not "know" behaviors.
 *
 * lift(Int)      ---> Behavior Int
 * lift(String)   ---> Behavior String
 * lift(t1 -> t2) ---> lift(t1) -> lift(t2)
 *
 * Consider f :: (t1 -> t2) -> t3.  Flapjax may apply f to a constant function
 * that returns a behavior.
 */
 


var mixedSwitchB = function(behaviourCreatorsB) {
  var init = valueNow(behaviourCreatorsB);
  
  var prevSourceE = null;
  
  var receiverE = internalE();

  
  var makerE = 
  createNode(
    [changes(behaviourCreatorsB)],
    function (p) {
      if (prevSourceE != null) {
        removeListener(prevSourceE, receiverE);
        prevSourceE = null;
      }
      if (p.value instanceof Behavior) {
        prevSourceE = changes(p.value);
        attachListener(prevSourceE, receiverE);
        sendEvent(receiverE, valueNow(p.value));
      }
      else {
        sendEvent(receiverE, p.value);
      }
    });
  
  if (init instanceof Behavior) {
    sendEvent(makerE, init);
  }
  
  return startsWith(
    receiverE,
    init instanceof Behavior? valueNow(init) : init);
};

var compilerInsertDomB = function(mixedB, target) {
  insertDomB(mixedSwitchB(mixedB),target,"over"); };

var compilerInsertValueB = function(mixedB,target,attrib) {
console.log(mixedB);
  if (typeof(mixedB) == "object") {
    for (var ix in mixedB) {
      if (Object.prototype && Object.prototype[ix]) {
        continue; }
      if (mixedB[ix] instanceof Behavior) {
        insertValueB(mixedSwitchB(mixedB[ix]),target,attrib,ix); }
      else {
        insertValueB(constantB(mixedB[ix]),target,attrib,ix); }};
  }
  else {
    insertValueB(mixedSwitchB(mixedB),target,attrib); }};


var compilerLift = function(f /* , args ... */) {
  checkBehavior: {
    for (var i = 0; i < arguments.length; i++) {
      if (arguments[i] instanceof Behavior) {
        break checkBehavior; } }
    return f.apply(this,slice(arguments,1));
  }

  // Assume some argument is a behavior.  This should always work.  We can
  // optimize later.
  var resultE = internalE();
  var r = liftB.apply(this,arguments);
  if (!(r instanceof Behavior)) {
    return r;
  }
  if (r.valueNow() instanceof EventStream) {
    return r.valueNow();
  }
  else {
    return mixedSwitchB(r);
  }
};

var compilerCall = function(f /* , args ... */) {
  return compilerLift.apply(this,arguments);
};
   

var compilerIf = function(test,cons,alt) {
  if (test instanceof Behavior) {
    return test.liftB(function(v) { return v ? cons : alt; }).switchB();
  }
  else {
    return test ? cons : alt;
  }
};
  

var unBehavior = function(recompute) { return function(v) {
  if (v instanceof Behavior) {
    if (v.valueNow() instanceof Behavior) {
      return unBehavior(recompute)(v.valueNow());
    }
    else {
      attachListener(v.changes(),recompute(v.changes()));
      return unBehavior()(v.valueNow());
    }
  }
  else if (typeof v == 'function') {
    return function() {
      var r = v.apply(this,arguments);
      return unBehavior(recompute)(r);
    }
  }
  else {
    return v;
  };
}};

// compilerEventStreamArg :: Behavior a -> a
var compilerEventStreamArg = function(x) {
  if (x instanceof Behavior) {
    return compilerEventStreamArg(x.valueNow()); }
  else if (typeof(x) == "function") {
    return function() {
      return compilerEventStreamArg(x.apply(this,arguments)); }}
  else {
    return x; }};


var compilerUnbehavior = function(v) {
  if (typeof v == 'function') {
    return function() {
      // These values may contain behaviors.
      var originalArgs = slice(arguments,0);

      var srcs = [ ];

      var recompute = function(src) {
        srcs.push(src);
        return recomputeE;
      };

      var resultE = internalE();
      
      var recomputeE = createNode([],function(send,_) {
        // Some argument changed.  We will recompute new values for all
        // arguments.
        map1(function(src) { removeListener(src,recomputeE); },srcs);
        srcs = [ ];
        var args = map1(unBehavior(recompute),originalArgs);
        var r = v.apply(this,args);
        sendEvent(resultE,r);
      });

      return resultE.startsWith(v.apply(this,map1(unBehavior(recompute),
                                              originalArgs)));
    }
  }
  else {
    return v;
  }
};
