var module = this;
var slice = __typedjs("\"flapjax.js\" (line 38, column 13)",
                      function (arr,start,stop)
                      {
                        var i,len = arr.length,r = [];
                        if (! stop)
                        {
                          stop = len;
                        };
                        if (stop < 0)
                        {
                          stop = len + stop;
                        };
                        if (start < 0)
                        {
                          start = len - start;
                        };
                        if (stop < start)
                        {
                          i = start;
                          start = stop;
                          stop = i;
                        };
                        for (i = 0; i < stop - start; i ++)
                        {
                          r[i] = arr[start + i];
                        };
                        return r;
                      });
var isEqual = __typedjs("\"flapjax.js\" (line 48, column 15)",
                        function (a,b)
                        {
                          return (a == b) || ((((typeof (a) == "number") && isNaN(a)) || a == "NaN") && (((typeof (b) == "number") && isNaN(b)) || b == "NaN"));
                        });
var forEach = __typedjs("\"flapjax.js\" (line 54, column 15)",
                        function (fn,arr)
                        {
                          for (var i = 0; i < arr.length; i ++)
                          {
                            fn(arr[i]);
                          };
                        });
var member = __typedjs("\"flapjax.js\" (line 61, column 14)",
                       function (elt,lst)
                       {
                         for (var i = 0; i < lst.length; i ++)
                         {
                           if (isEqual(lst[i],elt))
                           {
                             return true;
                           };
                         };
                         return false;
                       });
var zip = __typedjs("\"flapjax.js\" (line 68, column 11)",
                    function (arrays)
                    {
                      if (arrays.length == 0)
                      return [];
                      var ret = [];
                      for (var i = 0; i < arrays[0].length; i ++)
                      {
                        ret.push([]);
                        for (var j = 0; j < arrays.length; j ++)
                        ret[i].push(arrays[j][i]);
                      };
                      return ret;
                    });
var map1 = __typedjs("\"flapjax.js\" (line 80, column 12)",
                     function (f,src)
                     {
                       var dest = [];
                       for (var i = 0; i < src.length; i ++)
                       {
                         dest.push(f(src[i]));
                       };
                       return dest;
                     });
var map = __typedjs("\"flapjax.js\" (line 88, column 11)",
                    function (fn)
                    {
                      var arrays = slice(arguments,1);
                      if (arrays.length === 0)
                      {
                        return [];
                      }
                      else if (arrays.length === 1)
                           {
                             var ret = [];
                             for (var i = 0; i < arrays[0].length; i ++)
                             {
                               ret.push(fn(arrays[0][i]));
                             };
                             return ret;
                           }
                           else {
                                  var ret = zip(arrays);
                                  var o = new Object();
                                  for (var i = 0; i < ret.length; i ++)
                                  {
                                    ret[i] = fn.apply(o,ret[i]);
                                  };
                                  return ret;
                                };
                    });
var filter = __typedjs("\"flapjax.js\" (line 105, column 14)",
                       function (predFn,arr)
                       {
                         var res = [];
                         for (var i = 0; i < arr.length; i ++)
                         {
                           if (predFn(arr[i]))
                           {
                             res.push(arr[i]);
                           };
                         };
                         return res;
                       });
var fold = __typedjs("\"flapjax.js\" (line 115, column 12)",
                     function (fn,init)
                     {
                       var lists = slice(arguments,2);
                       if (lists.length === 0)
                       {
                         return init;
                       }
                       else if (lists.length === 1)
                            {
                              var acc = init;
                              for (var i = 0; i < lists[0].length; i ++)
                              {
                                acc = fn(lists[0][i],acc);
                              };
                              return acc;
                            }
                            else {
                                   var acc = init;
                                   for (var i = 0; i < lists[0].length; i ++)
                                   {
                                     var args = map(__typedjs("\"flapjax.js\" (line 128, column 41)",
                                                              function (lst)
                                                              {
                                                                return lst[i];
                                                              }),
                                                    lists);
                                     args.push(acc);
                                     acc = fn.apply({},args);
                                   };
                                   return acc;
                                 };
                     });
var foldR = __typedjs("\"flapjax.js\" (line 139, column 13)",
                      function (fn,init)
                      {
                        var lists = slice(arguments,2);
                        if (lists.length === 0)
                        {
                          return init;
                        }
                        else if (lists.length === 1)
                             {
                               var acc = init;
                               for (var i = lists[0].length - 1; i > - 1; i --)
                               acc = fn(lists[0][i],acc);
                               return acc;
                             }
                             else {
                                    var acc = init;
                                    for (var i = lists[0].length - 1; i > - 1; i --)
                                    {
                                      var args = map(__typedjs("\"flapjax.js\" (line 151, column 41)",
                                                               function (lst)
                                                               {
                                                                 return lst[i];
                                                               }),
                                                     lists);
                                      args.push(acc);
                                      acc = fn.apply({},args);
                                    };
                                    return acc;
                                  };
                      });
var doNotPropagate = {};
var Pulse = __typedjs("\"flapjax.js\" (line 170, column 13)",
                      function (stamp,value)
                      {
                        this.stamp = stamp;
                        this.value = value;
                      });
var PQ = __typedjs("\"flapjax.js\" (line 180, column 10)",
                   function ()
                   {
                     var ctx = this;
                     ctx.val = [];
                     this.insert = __typedjs("\"flapjax.js\" (line 183, column 17)",
                                             function (kv)
                                             {
                                               ctx.val.push(kv);
                                               var kvpos = ctx.val.length - 1;
                                               while (kvpos > 0 && kv.k < ctx.val[Math.floor((kvpos - 1) / 2)].k)
                                               {
                                                 var oldpos = kvpos;
                                                 kvpos = Math.floor((kvpos - 1) / 2);
                                                 ctx.val[oldpos] = ctx.val[kvpos];
                                                 ctx.val[kvpos] = kv;
                                               };
                                             });
                     this.isEmpty = __typedjs("\"flapjax.js\" (line 193, column 18)",
                                              function ()
                                              {
                                                return ctx.val.length === 0;
                                              });
                     this.peek = __typedjs("\"flapjax.js\" (line 197, column 15)",
                                           function ()
                                           {
                                             return ctx.val[0];
                                           });
                     this.pop = __typedjs("\"flapjax.js\" (line 201, column 14)",
                                          function ()
                                          {
                                            if (ctx.val.length == 1)
                                            {
                                              return ctx.val.pop();
                                            };
                                            var ret = ctx.val.shift();
                                            ctx.val.unshift(ctx.val.pop());
                                            var kvpos = 0;
                                            var kv = ctx.val[0];
                                            while (1)
                                            {
                                              var leftChild = (kvpos * 2 + 1 < ctx.val.length ? ctx.val[kvpos * 2 + 1].k : kv.k + 1);
                                              var rightChild = (kvpos * 2 + 2 < ctx.val.length ? ctx.val[kvpos * 2 + 2].k : kv.k + 1);
                                              if (leftChild > kv.k && rightChild > kv.k)
                                              break;
                                              if (leftChild < rightChild)
                                              {
                                                ctx.val[kvpos] = ctx.val[kvpos * 2 + 1];
                                                ctx.val[kvpos * 2 + 1] = kv;
                                                kvpos = kvpos * 2 + 1;
                                              }
                                              else {
                                                     ctx.val[kvpos] = ctx.val[kvpos * 2 + 2];
                                                     ctx.val[kvpos * 2 + 2] = kv;
                                                     kvpos = kvpos * 2 + 2;
                                                   };
                                            };
                                            return ret;
                                          });
                   });
var lastRank = 0;
var stamp = 1;
var nextStamp = __typedjs("\"flapjax.js\" (line 232, column 17)",
                          function ()
                          {
                            return ++ stamp;
                          });
var timeQueue = new PQ();
var currentlyPropagating = false;
var propagate = __typedjs("\"flapjax.js\" (line 239, column 17)",
                          function (now)
                          {
                            if (currentlyPropagating)
                            {
                              return;
                            };
                            currentlyPropagating = true;
                            try
                            {
                              var pulseQueue = timeQueue.peek();
                              if (pulseQueue && pulseQueue.k <= now)
                              {
                                pulseQueue = timeQueue.pop().pulses;
                                var nextPulse,i;
                                var len = 1;
                                while (len)
                                {
                                  var qv = pulseQueue.pop();
                                  len --;
                                  nextPulse = qv.n.updater(new Pulse(qv.v.stamp,qv.v.value));
                                  if (nextPulse != doNotPropagate)
                                  {
                                    for (i = 0; i < qv.n.sendsTo.length; i ++)
                                    {
                                      len ++;
                                      pulseQueue.insert({k: qv.n.sendsTo[i].rank, n: qv.n.sendsTo[i], v: nextPulse});
                                    };
                                  };
                                };
                                pulseQueue = timeQueue.peek();
                              };
                            }
                            finally{
                                     currentlyPropagating = false;
                                   };
                          });
var propagatePulse = __typedjs("\"flapjax.js\" (line 280, column 22)",
                               function (pulse,node,when)
                               {
                                 var now = (new Date()).getTime();
                                 var pulseQueue = new PQ();
                                 pulseQueue.insert({k: node.rank, n: node, v: pulse});
                                 when = when ? (now + when) : now;
                                 timeQueue.insert({k: when, pulses: pulseQueue});
                                 propagate(now);
                               });
var EventStream = __typedjs("\"flapjax.js\" (line 296, column 19)",
                            function (nodes,updater)
                            {
                              this.updater = updater;
                              this.sendsTo = [];
                              for (var i = 0; i < nodes.length; i ++)
                              {
                                nodes[i].sendsTo.push(this);
                              };
                              this.rank = ++ lastRank;
                            });
EventStream.prototype = new Object();
var createNode = __typedjs("\"flapjax.js\" (line 310, column 18)",
                           function (nodes,updater)
                           {
                             return new EventStream(nodes,updater);
                           });
var attachListener = __typedjs("\"flapjax.js\" (line 317, column 22)",
                               function (node,dependent)
                               {
                                 if (! (node instanceof EventStream))
                                 {
                                   throw "attachListenenerNode: expects event as first arg";
                                 };
                                 if (! (dependent instanceof EventStream))
                                 {
                                   throw "attachListenenerNode: expects event as second arg";
                                 };
                                 node.sendsTo.push(dependent);
                                 if (node.rank > dependent.rank)
                                 {
                                   var lowest = lastRank + 1;
                                   var q = [dependent];
                                   while (q.length)
                                   {
                                     var cur = q.splice(0,1)[0];
                                     cur.rank = ++ lastRank;
                                     q = q.concat(cur.sendsTo);
                                   };
                                 };
                               });
module.attachListener = attachListener;
var removeListener = __typedjs("\"flapjax.js\" (line 337, column 22)",
                               function (node,dependent)
                               {
                                 if (! (node instanceof EventStream))
                                 {
                                   throw "removeListenerNode: expects event as first arg";
                                 };
                                 if (! (dependent instanceof EventStream))
                                 {
                                   throw "removeListenenerNode: expects event as second arg";
                                 };
                                 var foundSending = false;
                                 for (var i = 0; i < node.sendsTo.length && ! foundSending; i ++)
                                 {
                                   if (node.sendsTo[i] == dependent)
                                   {
                                     node.sendsTo.splice(i,1);
                                     foundSending = true;
                                   };
                                 };
                                 return foundSending;
                               });
module.removeListener = removeListener;
var internalE = __typedjs("\"flapjax.js\" (line 357, column 17)",
                          function (dependsOn)
                          {
                            return createNode(dependsOn || [],
                                              __typedjs("\"flapjax.js\" (line 358, column 38)",
                                                        function (pulse)
                                                        {
                                                          return pulse;
                                                        }));
                          });
var zeroE = __typedjs("\"flapjax.js\" (line 361, column 13)",
                      function ()
                      {
                        return createNode([],
                                          __typedjs("\"flapjax.js\" (line 362, column 24)",
                                                    function (pulse)
                                                    {
                                                      throw ("zeroE : received a value; zeroE should not receive a value; the value was " + pulse.value);
                                                    }));
                      });
var oneE = __typedjs("\"flapjax.js\" (line 368, column 12)",
                     function (val)
                     {
                       var sent = false;
                       var evt = createNode([],
                                            __typedjs("\"flapjax.js\" (line 370, column 27)",
                                                      function (pulse)
                                                      {
                                                        if (sent)
                                                        {
                                                          throw ("oneE : received an extra value");
                                                        };
                                                        sent = true;
                                                        return pulse;
                                                      }));
                       window.setTimeout(__typedjs("\"flapjax.js\" (line 377, column 21)",
                                                   function ()
                                                   {
                                                     sendEvent(evt,val);
                                                   }),
                                         0);
                       return evt;
                     });
var mergeE = __typedjs("\"flapjax.js\" (line 383, column 14)",
                       function ()
                       {
                         if (arguments.length == 0)
                         {
                           return zeroE();
                         }
                         else {
                                var deps = slice(arguments,0);
                                return internalE(deps);
                              };
                       });
EventStream.prototype.mergeE = __typedjs("\"flapjax.js\" (line 394, column 32)",
                                         function ()
                                         {
                                           var deps = slice(arguments,0);
                                           deps.push(this);
                                           return internalE(deps);
                                         });
EventStream.prototype.constantE = __typedjs("\"flapjax.js\" (line 401, column 35)",
                                            function (constantValue)
                                            {
                                              return createNode([this],
                                                                __typedjs("\"flapjax.js\" (line 402, column 28)",
                                                                          function (pulse)
                                                                          {
                                                                            pulse.value = constantValue;
                                                                            return pulse;
                                                                          }));
                                            });
var constantE = __typedjs("\"flapjax.js\" (line 409, column 17)",
                          function (e,v)
                          {
                            return e.constantE(v);
                          });
var Behavior = __typedjs("\"flapjax.js\" (line 413, column 16)",
                         function (event,init,updater)
                         {
                           if (! (event instanceof EventStream))
                           {
                             throw "Behavior: expected event as second arg";
                           };
                           var behave = this;
                           this.last = init;
                           this.underlyingRaw = event;
                           this.underlying = createNode([event],
                                                        updater ? __typedjs("\"flapjax.js\" (line 427, column 7)",
                                                                            function (p)
                                                                            {
                                                                              behave.last = updater(p.value);
                                                                              p.value = behave.last;
                                                                              return p;
                                                                            }) : __typedjs("\"flapjax.js\" (line 431, column 7)",
                                                                                           function (p)
                                                                                           {
                                                                                             behave.last = p.value;
                                                                                             return p;
                                                                                           }));
                         });
Behavior.prototype = new Object();
var receiverE = __typedjs("\"flapjax.js\" (line 440, column 17)",
                          function ()
                          {
                            var evt = internalE();
                            evt.sendEvent = __typedjs("\"flapjax.js\" (line 442, column 19)",
                                                      function (value)
                                                      {
                                                        propagatePulse(new Pulse(nextStamp(),value),
                                                                       evt);
                                                      });
                            return evt;
                          });
var sendEvent = __typedjs("\"flapjax.js\" (line 450, column 17)",
                          function (node,value,when)
                          {
                            if (! (node instanceof EventStream))
                            {
                              throw "sendEvent: expected Event as first arg";
                            };
                            propagatePulse(new Pulse(nextStamp(),value),node,when);
                          });
EventStream.prototype.bindE = __typedjs("\"flapjax.js\" (line 457, column 31)",
                                        function (k)
                                        {
                                          var m = this;
                                          var prevE = false;
                                          var outE = createNode([],
                                                                __typedjs("\"flapjax.js\" (line 465, column 28)",
                                                                          function (pulse)
                                                                          {
                                                                            return pulse;
                                                                          }));
                                          outE.name = "bind outE";
                                          var inE = createNode([m],
                                                               __typedjs("\"flapjax.js\" (line 468, column 29)",
                                                                         function (pulse)
                                                                         {
                                                                           if (prevE)
                                                                           {
                                                                             removeListener(prevE,
                                                                                            outE);
                                                                           };
                                                                           prevE = k(pulse.value);
                                                                           if (prevE instanceof EventStream)
                                                                           {
                                                                             attachListener(prevE,
                                                                                            outE);
                                                                           }
                                                                           else {
                                                                                  throw "bindE : expected EventStream";
                                                                                };
                                                                           return doNotPropagate;
                                                                         }));
                                          inE.name = "bind inE";
                                          return outE;
                                        });
EventStream.prototype.mapE = __typedjs("\"flapjax.js\" (line 491, column 30)",
                                       function (f)
                                       {
                                         if (! (f instanceof Function))
                                         {
                                           throw ("mapE : expected a function as the first argument; received " + f);
                                         };
                                         return createNode([this],
                                                           __typedjs("\"flapjax.js\" (line 496, column 28)",
                                                                     function (pulse)
                                                                     {
                                                                       pulse.value = f(pulse.value);
                                                                       return pulse;
                                                                     }));
                                       });
EventStream.prototype.notE = __typedjs("\"flapjax.js\" (line 503, column 30)",
                                       function ()
                                       {
                                         return this.mapE(__typedjs("\"flapjax.js\" (line 503, column 60)",
                                                                    function (v)
                                                                    {
                                                                      return ! v;
                                                                    }));
                                       });
var notE = __typedjs("\"flapjax.js\" (line 506, column 12)",
                     function (e)
                     {
                       return e.notE();
                     });
EventStream.prototype.filterE = __typedjs("\"flapjax.js\" (line 509, column 33)",
                                          function (pred)
                                          {
                                            if (! (pred instanceof Function))
                                            {
                                              throw ("filterE : expected predicate; received " + pred);
                                            };
                                            return createNode([this],
                                                              __typedjs("\"flapjax.js\" (line 515, column 29)",
                                                                        function (pulse)
                                                                        {
                                                                          return pred(pulse.value) ? pulse : doNotPropagate;
                                                                        }));
                                          });
var filterE = __typedjs("\"flapjax.js\" (line 521, column 15)",
                        function (e,p)
                        {
                          return e.filterE(p);
                        });
EventStream.prototype.onceE = __typedjs("\"flapjax.js\" (line 525, column 31)",
                                        function ()
                                        {
                                          var done = false;
                                          return createNode([this],
                                                            __typedjs("\"flapjax.js\" (line 528, column 28)",
                                                                      function (pulse)
                                                                      {
                                                                        if (! done)
                                                                        {
                                                                          done = true;
                                                                          return pulse;
                                                                        }
                                                                        else {
                                                                               return doNotPropagate;
                                                                             };
                                                                      }));
                                        });
var onceE = __typedjs("\"flapjax.js\" (line 535, column 13)",
                      function (e)
                      {
                        return e.onceE();
                      });
EventStream.prototype.skipFirstE = __typedjs("\"flapjax.js\" (line 538, column 36)",
                                             function ()
                                             {
                                               var skipped = false;
                                               return createNode([this],
                                                                 __typedjs("\"flapjax.js\" (line 540, column 28)",
                                                                           function (pulse)
                                                                           {
                                                                             if (skipped)
                                                                             {
                                                                               return pulse;
                                                                             }
                                                                             else {
                                                                                    return doNotPropagate;
                                                                                  };
                                                                           }));
                                             });
var skipFirstE = __typedjs("\"flapjax.js\" (line 549, column 18)",
                           function (e)
                           {
                             return e.skipFirstE();
                           });
EventStream.prototype.collectE = __typedjs("\"flapjax.js\" (line 552, column 34)",
                                           function (init,fold)
                                           {
                                             var acc = init;
                                             return this.mapE(__typedjs("\"flapjax.js\" (line 555, column 5)",
                                                                        function (n)
                                                                        {
                                                                          var next = fold(n,acc);
                                                                          acc = next;
                                                                          return next;
                                                                        }));
                                           });
var collectE = __typedjs("\"flapjax.js\" (line 563, column 16)",
                         function (e,i,f)
                         {
                           return e.collectE(i,f);
                         });
EventStream.prototype.switchE = __typedjs("\"flapjax.js\" (line 567, column 33)",
                                          function ()
                                          {
                                            return this.bindE(__typedjs("\"flapjax.js\" (line 568, column 21)",
                                                                        function (v)
                                                                        {
                                                                          return v;
                                                                        }));
                                          });
var switchE = __typedjs("\"flapjax.js\" (line 572, column 15)",
                        function (e)
                        {
                          return e.switchE();
                        });
EventStream.prototype.ifE = __typedjs("\"flapjax.js\" (line 575, column 29)",
                                      function (thenE,elseE)
                                      {
                                        var testStamp = - 1;
                                        var testValue = false;
                                        createNode([this],
                                                   __typedjs("\"flapjax.js\" (line 579, column 21)",
                                                             function (pulse)
                                                             {
                                                               testStamp = pulse.stamp;
                                                               testValue = pulse.value;
                                                               return doNotPropagate;
                                                             }));
                                        return mergeE(createNode([thenE],
                                                                 __typedjs("\"flapjax.js\" (line 581, column 36)",
                                                                           function (pulse)
                                                                           {
                                                                             if (testValue && (testStamp == pulse.stamp))
                                                                             {
                                                                               send(pulse);
                                                                             };
                                                                           })),
                                                      createNode([elseE],
                                                                 __typedjs("\"flapjax.js\" (line 582, column 24)",
                                                                           function (pulse)
                                                                           {
                                                                             if (! testValue && (testStamp == pulse.stamp))
                                                                             {
                                                                               send(pulse);
                                                                             };
                                                                           })));
                                      });
var ifE = __typedjs("\"flapjax.js\" (line 586, column 11)",
                    function (test,thenE,elseE)
                    {
                      if (test instanceof EventStream)
                      {
                        return test.ifE(thenE,elseE);
                      }
                      else {
                             return test ? thenE : elseE;
                           };
                    });
var andE = __typedjs("\"flapjax.js\" (line 594, column 12)",
                     function ()
                     {
                       var nodes = slice(arguments,0);
                       var acc = (nodes.length > 0) ? nodes[nodes.length - 1] : oneE(true);
                       for (var i = nodes.length - 2; i > - 1; i --)
                       {
                         acc = ifE(nodes[i],acc,nodes[i].constantE(false));
                       };
                       return acc;
                     });
EventStream.prototype.andE = __typedjs("\"flapjax.js\" (line 610, column 30)",
                                       function ()
                                       {
                                         var deps = [this].concat(slice(arguments,0));
                                         return andE.apply(this,deps);
                                       });
var orE = __typedjs("\"flapjax.js\" (line 616, column 11)",
                    function ()
                    {
                      var nodes = slice(arguments,0);
                      var acc = (nodes.length > 2) ? nodes[nodes.length - 1] : oneE(false);
                      for (var i = nodes.length - 2; i > - 1; i --)
                      {
                        acc = ifE(nodes[i],nodes[i],acc);
                      };
                      return acc;
                    });
EventStream.prototype.orE = __typedjs("\"flapjax.js\" (line 630, column 29)",
                                      function ()
                                      {
                                        var deps = [this].concat(slice(arguments,0));
                                        return orE.apply(this,deps);
                                      });
var delayStaticE = __typedjs("\"flapjax.js\" (line 636, column 20)",
                             function (event,time)
                             {
                               updateResolution(time);
                               var resE = internalE();
                               createNode([event],
                                          __typedjs("\"flapjax.js\" (line 640, column 23)",
                                                    function (p)
                                                    {
                                                      sendEvent(resE,p.value,time);
                                                      return doNotPropagate;
                                                    }));
                               return resE;
                             });
EventStream.prototype.delayE = __typedjs("\"flapjax.js\" (line 649, column 32)",
                                         function (time)
                                         {
                                           var event = this;
                                           if (time instanceof Behavior)
                                           {
                                             var receiverEE = internalE();
                                             var link = {from: event, towards: delayStaticE(event,
                                                                                            valueNow(time))};
                                             var switcherE = createNode([changes(time)],
                                                                        __typedjs("\"flapjax.js\" (line 665, column 7)",
                                                                                  function (p)
                                                                                  {
                                                                                    removeListener(link.from,
                                                                                                   link.towards);
                                                                                    link = {from: event, towards: delayStaticE(event,
                                                                                                                               p.value)};
                                                                                    sendEvent(receiverEE,
                                                                                              link.towards);
                                                                                    return doNotPropagate;
                                                                                  }));
                                             var resE = receiverEE.switchE();
                                             sendEvent(switcherE,valueNow(time));
                                             return resE;
                                           }
                                           else {
                                                  return delayStaticE(event,time);
                                                };
                                         });
var delayE = __typedjs("\"flapjax.js\" (line 685, column 14)",
                       function (sourceE,interval)
                       {
                         return sourceE.delayE(interval);
                       });
var mapE = __typedjs("\"flapjax.js\" (line 691, column 12)",
                     function (fn)
                     {
                       var valsOrNodes = slice(arguments,0);
                       var selectors = [];
                       var selectI = 0;
                       var nodes = [];
                       for (var i = 0; i < valsOrNodes.length; i ++)
                       {
                         if (valsOrNodes[i] instanceof EventStream)
                         {
                           nodes.push(valsOrNodes[i]);
                           selectors.push((__typedjs("\"flapjax.js\" (line 703, column 10)",
                                                     function (ii)
                                                     {
                                                       return __typedjs("\"flapjax.js\" (line 704, column 20)",
                                                                        function (realArgs)
                                                                        {
                                                                          return realArgs[ii];
                                                                        });
                                                     }))(selectI));
                           selectI ++;
                         }
                         else {
                                selectors.push((__typedjs("\"flapjax.js\" (line 711, column 10)",
                                                          function (aa)
                                                          {
                                                            return __typedjs("\"flapjax.js\" (line 712, column 20)",
                                                                             function ()
                                                                             {
                                                                               return aa;
                                                                             });
                                                          }))(valsOrNodes[i]));
                              };
                       };
                       var context = this;
                       var nofnodes = slice(selectors,1);
                       if (nodes.length === 0)
                       {
                         return oneE(fn.apply(context,valsOrNodes));
                       }
                       else if ((nodes.length === 1) && (fn instanceof Function))
                            {
                              return nodes[0].mapE(__typedjs("\"flapjax.js\" (line 726, column 7)",
                                                             function ()
                                                             {
                                                               var args = arguments;
                                                               return fn.apply(context,
                                                                               map(__typedjs("\"flapjax.js\" (line 730, column 15)",
                                                                                             function (s)
                                                                                             {
                                                                                               return s(args);
                                                                                             }),
                                                                                   nofnodes));
                                                             }));
                            }
                            else if (nodes.length === 1)
                                 {
                                   return fn.mapE(__typedjs("\"flapjax.js\" (line 734, column 7)",
                                                            function (v)
                                                            {
                                                              var args = arguments;
                                                              return v.apply(context,
                                                                             map(__typedjs("\"flapjax.js\" (line 738, column 15)",
                                                                                           function (s)
                                                                                           {
                                                                                             return s(args);
                                                                                           }),
                                                                                 nofnodes));
                                                            }));
                                 }
                                 else if (fn instanceof Function)
                                      {
                                        return createTimeSyncNode(nodes).mapE(__typedjs("\"flapjax.js\" (line 742, column 7)",
                                                                                        function (arr)
                                                                                        {
                                                                                          return fn.apply(this,
                                                                                                          map(__typedjs("\"flapjax.js\" (line 745, column 15)",
                                                                                                                        function (s)
                                                                                                                        {
                                                                                                                          return s(arr);
                                                                                                                        }),
                                                                                                              nofnodes));
                                                                                        }));
                                      }
                                      else if (fn instanceof EventStream)
                                           {
                                             return createTimeSyncNode(nodes).mapE(__typedjs("\"flapjax.js\" (line 749, column 7)",
                                                                                             function (arr)
                                                                                             {
                                                                                               return arr[0].apply(this,
                                                                                                                   map(__typedjs("\"flapjax.js\" (line 752, column 15)",
                                                                                                                                 function (s)
                                                                                                                                 {
                                                                                                                                   return s(arr);
                                                                                                                                 }),
                                                                                                                       nofnodes));
                                                                                             }));
                                           }
                                           else {
                                                  throw "unknown mapE case";
                                                };
                     });
EventStream.prototype.snapshotE = __typedjs("\"flapjax.js\" (line 758, column 35)",
                                            function (valueB)
                                            {
                                              return createNode([this],
                                                                __typedjs("\"flapjax.js\" (line 759, column 29)",
                                                                          function (pulse)
                                                                          {
                                                                            pulse.value = valueNow(valueB);
                                                                            return pulse;
                                                                          }));
                                            });
var snapshotE = __typedjs("\"flapjax.js\" (line 766, column 17)",
                          function (triggerE,valueB)
                          {
                            return triggerE.snapshotE(valueB);
                          });
EventStream.prototype.filterRepeatsE = __typedjs("\"flapjax.js\" (line 771, column 40)",
                                                 function (optStart)
                                                 {
                                                   var hadFirst = optStart === undefined ? false : true;
                                                   var prev = optStart;
                                                   return this.filterE(__typedjs("\"flapjax.js\" (line 775, column 23)",
                                                                                 function (v)
                                                                                 {
                                                                                   if (! hadFirst || ! (isEqual(prev,
                                                                                                                v)))
                                                                                   {
                                                                                     hadFirst = true;
                                                                                     prev = v;
                                                                                     return true;
                                                                                   }
                                                                                   else {
                                                                                          return false;
                                                                                        };
                                                                                 }));
                                                 });
var filterRepeatsE = __typedjs("\"flapjax.js\" (line 788, column 22)",
                               function (sourceE,optStart)
                               {
                                 return sourceE.filterRepeatsE(optStart);
                               });
var calmStaticE = __typedjs("\"flapjax.js\" (line 794, column 19)",
                            function (triggerE,time)
                            {
                              var out = internalE();
                              createNode([triggerE],
                                         __typedjs("\"flapjax.js\" (line 798, column 5)",
                                                   function ()
                                                   {
                                                     var towards = null;
                                                     return __typedjs("\"flapjax.js\" (line 800, column 14)",
                                                                      function (p)
                                                                      {
                                                                        if (towards !== null)
                                                                        {
                                                                          clearTimeout(towards);
                                                                        };
                                                                        towards = setTimeout(__typedjs("\"flapjax.js\" (line 802, column 31)",
                                                                                                       function ()
                                                                                                       {
                                                                                                         towards = null;
                                                                                                         sendEvent(out,
                                                                                                                   p.value);
                                                                                                       }),
                                                                                             time);
                                                                        return doNotPropagate;
                                                                      });
                                                   })());
                              return out;
                            });
EventStream.prototype.calmE = __typedjs("\"flapjax.js\" (line 810, column 31)",
                                        function (time)
                                        {
                                          if (time instanceof Behavior)
                                          {
                                            var out = internalE();
                                            createNode([this],
                                                       __typedjs("\"flapjax.js\" (line 815, column 7)",
                                                                 function ()
                                                                 {
                                                                   var towards = null;
                                                                   return __typedjs("\"flapjax.js\" (line 817, column 16)",
                                                                                    function (p)
                                                                                    {
                                                                                      if (towards !== null)
                                                                                      {
                                                                                        clearTimeout(towards);
                                                                                      };
                                                                                      towards = setTimeout(__typedjs("\"flapjax.js\" (line 819, column 33)",
                                                                                                                     function ()
                                                                                                                     {
                                                                                                                       towards = null;
                                                                                                                       sendEvent(out,
                                                                                                                                 p.value);
                                                                                                                     }),
                                                                                                           valueNow(time));
                                                                                      return doNotPropagate;
                                                                                    });
                                                                 })());
                                            return out;
                                          }
                                          else {
                                                 return calmStaticE(this,time);
                                               };
                                        });
var calmE = __typedjs("\"flapjax.js\" (line 830, column 13)",
                      function (sourceE,interval)
                      {
                        return sourceE.calmE(interval);
                      });
EventStream.prototype.blindE = __typedjs("\"flapjax.js\" (line 835, column 32)",
                                         function (time)
                                         {
                                           return createNode([this],
                                                             __typedjs("\"flapjax.js\" (line 838, column 5)",
                                                                       function ()
                                                                       {
                                                                         var intervalFn = time instanceof Behavior ? __typedjs("\"flapjax.js\" (line 841, column 7)",
                                                                                                                               function ()
                                                                                                                               {
                                                                                                                                 return valueNow(time);
                                                                                                                               }) : __typedjs("\"flapjax.js\" (line 842, column 9)",
                                                                                                                                              function ()
                                                                                                                                              {
                                                                                                                                                return time;
                                                                                                                                              });
                                                                         var lastSent = (new Date()).getTime() - intervalFn() - 1;
                                                                         return __typedjs("\"flapjax.js\" (line 844, column 14)",
                                                                                          function (p)
                                                                                          {
                                                                                            var curTime = (new Date()).getTime();
                                                                                            if (curTime - lastSent > intervalFn())
                                                                                            {
                                                                                              lastSent = curTime;
                                                                                              return p;
                                                                                            }
                                                                                            else {
                                                                                                   return doNotPropagate;
                                                                                                 };
                                                                                          });
                                                                       })());
                                         });
var blindE = __typedjs("\"flapjax.js\" (line 856, column 14)",
                       function (sourceE,interval)
                       {
                         return sourceE.blindE(interval);
                       });
EventStream.prototype.startsWith = __typedjs("\"flapjax.js\" (line 861, column 36)",
                                             function (init)
                                             {
                                               return new Behavior(this,init);
                                             });
var startsWith = __typedjs("\"flapjax.js\" (line 866, column 18)",
                           function (e,init)
                           {
                             if (! (e instanceof EventStream))
                             {
                               throw "startsWith: expected EventStream; received " + e;
                             };
                             return e.startsWith(init);
                           });
Behavior.prototype.valueNow = __typedjs("\"flapjax.js\" (line 874, column 31)",
                                        function ()
                                        {
                                          return this.last;
                                        });
var valueNow = __typedjs("\"flapjax.js\" (line 877, column 16)",
                         function (behavior)
                         {
                           return behavior.valueNow();
                         });
Behavior.prototype.changes = __typedjs("\"flapjax.js\" (line 880, column 30)",
                                       function ()
                                       {
                                         return this.underlying;
                                       });
var changes = __typedjs("\"flapjax.js\" (line 885, column 15)",
                        function (behave)
                        {
                          return behave.changes();
                        });
Behavior.prototype.switchB = __typedjs("\"flapjax.js\" (line 888, column 30)",
                                       function ()
                                       {
                                         var behaviourCreatorsB = this;
                                         var init = valueNow(behaviourCreatorsB);
                                         var prevSourceE = null;
                                         var receiverE = new internalE();
                                         var makerE = createNode([changes(behaviourCreatorsB)],
                                                                 __typedjs("\"flapjax.js\" (line 900, column 5)",
                                                                           function (p)
                                                                           {
                                                                             if (! (p.value instanceof Behavior))
                                                                             {
                                                                               throw "switchB: expected Behavior as value of Behavior of first argument";
                                                                             };
                                                                             if (prevSourceE != null)
                                                                             {
                                                                               removeListener(prevSourceE,
                                                                                              receiverE);
                                                                             };
                                                                             prevSourceE = changes(p.value);
                                                                             attachListener(prevSourceE,
                                                                                            receiverE);
                                                                             sendEvent(receiverE,
                                                                                       valueNow(p.value));
                                                                             return doNotPropagate;
                                                                           }));
                                         if (init instanceof Behavior)
                                         {
                                           sendEvent(makerE,init);
                                         };
                                         return startsWith(receiverE,
                                                           init instanceof Behavior ? valueNow(init) : init);
                                       });
var switchB = __typedjs("\"flapjax.js\" (line 923, column 15)",
                        function (b)
                        {
                          return b.switchB();
                        });
var timerB = __typedjs("\"flapjax.js\" (line 927, column 14)",
                       function (interval)
                       {
                         return startsWith(timerE(interval),(new Date()).getTime());
                       });
var delayStaticB = __typedjs("\"flapjax.js\" (line 933, column 20)",
                             function (triggerB,time,init)
                             {
                               return startsWith(delayStaticE(changes(triggerB),time),init);
                             });
Behavior.prototype.delayB = __typedjs("\"flapjax.js\" (line 938, column 29)",
                                      function (time,init)
                                      {
                                        var triggerB = this;
                                        if (time instanceof Behavior)
                                        {
                                          return startsWith(delayE(changes(triggerB),time),
                                                            arguments.length > 3 ? init : valueNow(triggerB));
                                        }
                                        else {
                                               return delayStaticB(triggerB,
                                                                   time,
                                                                   arguments.length > 3 ? init : valueNow(triggerB));
                                             };
                                      });
var delayB = __typedjs("\"flapjax.js\" (line 955, column 14)",
                       function (srcB,timeB,init)
                       {
                         return srcB.delayB(timeB,init);
                       });
Behavior.prototype.sendBehavior = __typedjs("\"flapjax.js\" (line 962, column 35)",
                                            function (val)
                                            {
                                              sendEvent(this.underlyingRaw,val);
                                            });
Behavior.prototype.sendBehavior = Behavior.prototype.sendBehavior;
var sendBehavior = __typedjs("\"flapjax.js\" (line 967, column 20)",
                             function (b,v)
                             {
                               b.sendBehavior(v);
                             });
Behavior.prototype.ifB = __typedjs("\"flapjax.js\" (line 971, column 26)",
                                   function (trueB,falseB)
                                   {
                                     var testB = this;
                                     if (! (trueB instanceof Behavior))
                                     {
                                       trueB = constantB(trueB);
                                     };
                                     if (! (falseB instanceof Behavior))
                                     {
                                       falseB = constantB(falseB);
                                     };
                                     return liftB(__typedjs("\"flapjax.js\" (line 976, column 16)",
                                                            function (te,t,f)
                                                            {
                                                              return te ? t : f;
                                                            }),
                                                  testB,
                                                  trueB,
                                                  falseB);
                                   });
var ifB = __typedjs("\"flapjax.js\" (line 980, column 11)",
                    function (test,cons,altr)
                    {
                      if (! (test instanceof Behavior))
                      {
                        test = constantB(test);
                      };
                      return test.ifB(cons,altr);
                    });
var condB = __typedjs("\"flapjax.js\" (line 989, column 13)",
                      function ()
                      {
                        var pairs = slice(arguments,0);
                        return liftB.apply({},
                                           [__typedjs("\"flapjax.js\" (line 991, column 24)",
                                                      function ()
                                                      {
                                                        for (var i = 0; i < pairs.length; i ++)
                                                        {
                                                          if (arguments[i])
                                                          return arguments[pairs.length + i];
                                                        };
                                                        return undefined;
                                                      })].concat(map(__typedjs("\"flapjax.js\" (line 996, column 23)",
                                                                               function (pair)
                                                                               {
                                                                                 return pair[0];
                                                                               }),
                                                                     pairs).concat(map(__typedjs("\"flapjax.js\" (line 996, column 74)",
                                                                                                 function (pair)
                                                                                                 {
                                                                                                   return pair[1];
                                                                                                 }),
                                                                                       pairs))));
                      });
var constantB = __typedjs("\"flapjax.js\" (line 1002, column 17)",
                          function (val)
                          {
                            return new Behavior(internalE(),val);
                          });
var liftB = __typedjs("\"flapjax.js\" (line 1007, column 13)",
                      function (fn)
                      {
                        var args = slice(arguments,1);
                        var constituentsE = map(changes,
                                                filter(__typedjs("\"flapjax.js\" (line 1014, column 12)",
                                                                 function (v)
                                                                 {
                                                                   return v instanceof Behavior;
                                                                 }),
                                                       arguments));
                        var getCur = __typedjs("\"flapjax.js\" (line 1018, column 16)",
                                               function (v)
                                               {
                                                 return v instanceof Behavior ? v.last : v;
                                               });
                        var ctx = this;
                        var getRes = __typedjs("\"flapjax.js\" (line 1023, column 16)",
                                               function ()
                                               {
                                                 return getCur(fn).apply(ctx,map(getCur,args));
                                               });
                        if (constituentsE.length == 1)
                        {
                          return new Behavior(constituentsE[0],getRes(),getRes);
                        };
                        var prevStamp = - 1;
                        var mid = createNode(constituentsE,
                                             __typedjs("\"flapjax.js\" (line 1033, column 39)",
                                                       function (p)
                                                       {
                                                         if (p.stamp != prevStamp)
                                                         {
                                                           prevStamp = p.stamp;
                                                           return p;
                                                         }
                                                         else {
                                                                return doNotPropagate;
                                                              };
                                                       }));
                        return new Behavior(mid,getRes(),getRes);
                      });
Behavior.prototype.liftB = __typedjs("\"flapjax.js\" (line 1047, column 28)",
                                     function ()
                                     {
                                       var args = slice(arguments,0).concat([this]);
                                       return liftB.apply(this,args);
                                     });
var andB = __typedjs("\"flapjax.js\" (line 1053, column 12)",
                     function ()
                     {
                       return liftB.apply({},
                                          [__typedjs("\"flapjax.js\" (line 1054, column 24)",
                                                     function ()
                                                     {
                                                       for (var i = 0; i < arguments.length; i ++)
                                                       {
                                                         if (! arguments[i])
                                                         return false;
                                                       };
                                                       return true;
                                                     })].concat(slice(arguments,0)));
                     });
Behavior.prototype.andB = __typedjs("\"flapjax.js\" (line 1061, column 27)",
                                    function ()
                                    {
                                      return andB([this].concat(arguments));
                                    });
var orB = __typedjs("\"flapjax.js\" (line 1066, column 11)",
                    function ()
                    {
                      return liftB.apply({},
                                         [__typedjs("\"flapjax.js\" (line 1067, column 24)",
                                                    function ()
                                                    {
                                                      for (var i = 0; i < arguments.length; i ++)
                                                      {
                                                        if (arguments[i])
                                                        return true;
                                                      };
                                                      return false;
                                                    })].concat(slice(arguments,0)));
                    });
Behavior.prototype.orB = __typedjs("\"flapjax.js\" (line 1074, column 26)",
                                   function ()
                                   {
                                     return orB([this].concat(arguments));
                                   });
Behavior.prototype.notB = __typedjs("\"flapjax.js\" (line 1079, column 27)",
                                    function ()
                                    {
                                      return this.liftB(__typedjs("\"flapjax.js\" (line 1080, column 21)",
                                                                  function (v)
                                                                  {
                                                                    return ! v;
                                                                  }));
                                    });
var notB = __typedjs("\"flapjax.js\" (line 1084, column 12)",
                     function (b)
                     {
                       return b.notB();
                     });
Behavior.prototype.blindB = __typedjs("\"flapjax.js\" (line 1087, column 29)",
                                      function (intervalB)
                                      {
                                        return changes(this).blindE(intervalB).startsWith(this.valueNow());
                                      });
var blindB = __typedjs("\"flapjax.js\" (line 1092, column 14)",
                       function (srcB,intervalB)
                       {
                         return srcB.blindB(intervalB);
                       });
Behavior.prototype.calmB = __typedjs("\"flapjax.js\" (line 1097, column 28)",
                                     function (intervalB)
                                     {
                                       return this.changes().calmE(intervalB).startsWith(this.valueNow());
                                     });
var calmB = __typedjs("\"flapjax.js\" (line 1102, column 13)",
                      function (srcB,intervalB)
                      {
                        return srcB.calmB(intervalB);
                      });
var addEvent = __typedjs("\"flapjax.js\" (line 1114, column 16)",
                         function (obj,evType,fn)
                         {
                           if (obj.addEventListener)
                           {
                             obj.addEventListener(evType,fn,false);
                             return true;
                           }
                           else if (obj.attachEvent)
                                {
                                  return obj.attachEvent("on" + evType,fn);
                                }
                                else {
                                       return false;
                                     };
                         });
var getElementsByClass = __typedjs("\"flapjax.js\" (line 1133, column 26)",
                                   function (searchClass,node,tag)
                                   {
                                     var classElements = [];
                                     if ((node === null) || (node === undefined))
                                     {
                                       node = document;
                                     };
                                     if ((tag === null) || (tag === undefined))
                                     {
                                       tag = "*";
                                     };
                                     var els = node.getElementsByTagName(tag);
                                     var elsLen = els.length;
                                     var pattern = new RegExp("(^|\\s)" + searchClass + "(\\s|$)");
                                     for (var i = 0,j = 0; i < elsLen; i ++)
                                     {
                                       if (pattern.test(els[i].className))
                                       {
                                         classElements.push(els[i]);
                                       };
                                     };
                                     return classElements;
                                   });
var swapDom = __typedjs("\"flapjax.js\" (line 1151, column 15)",
                        function (replaceMe,withMe)
                        {
                          if ((replaceMe === null) || (replaceMe === undefined))
                          {
                            throw ("swapDom: expected dom node or id, received: " + replaceMe);
                          };
                          var replaceMeD = getObj(replaceMe);
                          if (! (replaceMeD.nodeType > 0))
                          {
                            throw ("swapDom expected a Dom node as first arg, received " + replaceMeD);
                          };
                          if (withMe)
                          {
                            var withMeD = getObj(withMe);
                            if (! (withMeD.nodeType > 0))
                            {
                              throw "swapDom: can only swap with a DOM object";
                            };
                            try
                            {
                              if (replaceMeD.parentNode == null)
                              {
                                return withMeD;
                              };
                              if (withMeD != replaceMeD)
                              replaceMeD.parentNode.replaceChild(withMeD,replaceMeD);
                            }
                            catch (e) {
                                        throw ("swapDom error in replace call: withMeD: " + withMeD + ", replaceMe Parent: " + replaceMeD + ", " + e + ", parent: " + replaceMeD.parentNode);
                                      };
                          }
                          else {
                                 replaceMeD.parentNode.removeChild(replaceMeD);
                               };
                          return replaceMeD;
                        });
var getObj = __typedjs("\"flapjax.js\" (line 1180, column 14)",
                       function (name)
                       {
                         if (typeof (name) == "object")
                         {
                           return name;
                         }
                         else if ((typeof (name) == "null") || (typeof (name) == "undefined"))
                              {
                                throw "getObj: expects a Dom obj or Dom id as first arg";
                              }
                              else {
                                     var res = document.getElementById ? document.getElementById(name) : document.all ? document.all[name] : document.layers ? document.layers[name] : (__typedjs("\"flapjax.js\" (line 1190, column 18)",
                                                                                                                                                                                                  function ()
                                                                                                                                                                                                  {
                                                                                                                                                                                                    throw "getObj: flapjax: cannot access object";
                                                                                                                                                                                                  }))();
                                     if ((res === null) || (res === undefined))
                                     {
                                       throw ("getObj: no obj to get: " + name);
                                     };
                                     return res;
                                   };
                       });
var $ = getObj;
var getMostDom = __typedjs("\"flapjax.js\" (line 1207, column 18)",
                           function (domObj,indices)
                           {
                             var acc = getObj(domObj);
                             if ((indices === null) || (indices === undefined) || (indices.length < 1))
                             {
                               return acc;
                             }
                             else {
                                    for (var i = 0; i < indices.length - 1; i ++)
                                    {
                                      acc = acc[indices[i]];
                                    };
                                    return acc;
                                  };
                           });
var getDomVal = __typedjs("\"flapjax.js\" (line 1219, column 17)",
                          function (domObj,indices)
                          {
                            var val = getMostDom(domObj,indices);
                            if (indices && indices.length > 0)
                            {
                              val = val[indices[indices.length - 1]];
                            };
                            return val;
                          });
var ___timerID = 0;
var __getTimerId = __typedjs("\"flapjax.js\" (line 1230, column 20)",
                             function ()
                             {
                               return ++ ___timerID;
                             });
var timerDisablers = [];
var disableTimerNode = __typedjs("\"flapjax.js\" (line 1233, column 24)",
                                 function (node)
                                 {
                                   timerDisablers[node.__timerId]();
                                 });
var disableTimer = __typedjs("\"flapjax.js\" (line 1235, column 20)",
                             function (v)
                             {
                               if (v instanceof Behavior)
                               {
                                 disableTimerNode(v.underlyingRaw);
                               }
                               else if (v instanceof EventStream)
                                    {
                                      disableTimerNode(v);
                                    };
                             });
var currentSchedulerResolution = 500;
var schedulerID;
var delayedScheduler = __typedjs("\"flapjax.js\" (line 1246, column 24)",
                                 function ()
                                 {
                                   propagate((new Date()).getTime());
                                 });
var updateResolution = __typedjs("\"flapjax.js\" (line 1250, column 24)",
                                 function (newResolution)
                                 {
                                   if (newResolution < currentSchedulerResolution)
                                   {
                                     clearInterval(schedulerID);
                                     currentSchedulerResolution = newResolution;
                                     schedulerID = setInterval(delayedScheduler,newResolution);
                                   };
                                 });
schedulerID = setInterval(delayedScheduler,
                          currentSchedulerResolution);
var createTimerNodeStatic = __typedjs("\"flapjax.js\" (line 1262, column 29)",
                                      function (interval)
                                      {
                                        updateResolution(interval);
                                        var disabled = false;
                                        var node = createNode([],
                                                              __typedjs("\"flapjax.js\" (line 1267, column 29)",
                                                                        function (pulse)
                                                                        {
                                                                          if (! disabled)
                                                                          {
                                                                            sendEvent(node,
                                                                                      (new Date()).getTime() + interval,
                                                                                      interval);
                                                                            return pulse;
                                                                          }
                                                                          else {
                                                                                 return doNotPropagate;
                                                                               };
                                                                        }));
                                        var localSchedulerID = setInterval(delayedScheduler,
                                                                           interval);
                                        node.__timerId = __getTimerId();
                                        timerDisablers[node.__timerId] = __typedjs("\"flapjax.js\" (line 1281, column 36)",
                                                                                   function ()
                                                                                   {
                                                                                     clearInterval(localSchedulerID);
                                                                                     disabled = true;
                                                                                   });
                                        sendEvent(node,(new Date()).getTime(),interval);
                                        return node;
                                      });
var timerE = __typedjs("\"flapjax.js\" (line 1293, column 14)",
                       function (interval)
                       {
                         if (interval instanceof Behavior)
                         {
                           var receiverE = internalE();
                           var res = receiverE.switchE();
                           var prevE = createTimerNodeStatic(valueNow(interval));
                           sendEvent(receiverE,prevE);
                           createNode([changes(interval)],
                                      __typedjs("\"flapjax.js\" (line 1309, column 7)",
                                                function (p)
                                                {
                                                  disableTimerNode(prevE);
                                                  prevE = createTimerNodeStatic(p.value);
                                                  sendEvent(receiverE,prevE);
                                                  return doNotPropagate;
                                                }));
                           res.__timerId = __getTimerId();
                           timerDisablers[res.__timerId] = __typedjs("\"flapjax.js\" (line 1317, column 37)",
                                                                     function ()
                                                                     {
                                                                       disableTimerNode[prevE]();
                                                                       return;
                                                                     });
                           return res;
                         }
                         else {
                                return createTimerNodeStatic(interval);
                              };
                       });
var deepEach = __typedjs("\"flapjax.js\" (line 1330, column 16)",
                         function (arr,f)
                         {
                           for (var i = 0; i < arr.length; i ++)
                           {
                             if (arr[i] instanceof Array)
                             {
                               deepEach(arr[i],f);
                             }
                             else {
                                    f(arr[i]);
                                  };
                           };
                         });
var mapWithKeys = __typedjs("\"flapjax.js\" (line 1342, column 19)",
                            function (obj,f)
                            {
                              for (var ix in obj)
                              {
                                if (! (Object.prototype && Object.prototype[ix] == obj[ix]))
                                {
                                  f(ix,obj[ix]);
                                };
                              };
                            });
var insertAfter = __typedjs("\"flapjax.js\" (line 1351, column 19)",
                            function (parent,newChild,refChild)
                            {
                              if (refChild.nextSibling)
                              {
                                parent.insertBefore(newChild,refChild.nextSibling);
                              }
                              else {
                                     parent.appendChild(newChild);
                                   };
                            });
var swapChildren = __typedjs("\"flapjax.js\" (line 1362, column 20)",
                             function (parent,existingChildren,newChildren)
                             {
                               var end = Math.min(existingChildren.length,newChildren.length);
                               var i;
                               for (i = 0; i < end; i ++)
                               {
                                 parent.replaceChild(newChildren[i],existingChildren[i]);
                               };
                               var lastInsertedChild = existingChildren[i - 1];
                               if (end < existingChildren.length)
                               {
                                 for (i = end; i < existingChildren.length; i ++)
                                 {
                                   parent.removeChild(existingChildren[i]);
                                 };
                               }
                               else if (end < newChildren.length)
                                    {
                                      for (i = end; i < newChildren.length; i ++)
                                      {
                                        insertAfter(parent,newChildren[i],newChildren[i - 1]);
                                      };
                                    };
                             });
var elementize = __typedjs("\"flapjax.js\" (line 1385, column 35)",
                           function (maybeElement)
                           {
                             return (maybeElement.nodeType > 0) ? maybeElement : document.createTextNode(maybeElement.toString());
                           });
var staticEnstyle = __typedjs("\"flapjax.js\" (line 1392, column 21)",
                              function (obj,prop,val)
                              {
                                if (val instanceof Object)
                                {
                                  mapWithKeys(val,
                                              __typedjs("\"flapjax.js\" (line 1394, column 22)",
                                                        function (k,v)
                                                        {
                                                          enstyle(obj[prop],k,v);
                                                        }));
                                }
                                else {
                                       obj[prop] = val;
                                     };
                              });
var dynamicEnstyle = __typedjs("\"flapjax.js\" (line 1402, column 22)",
                               function (obj,prop,val)
                               {
                                 if (val instanceof Behavior)
                                 {
                                   staticEnstyle(obj,prop,val.valueNow());
                                   val.liftB(__typedjs("\"flapjax.js\" (line 1405, column 15)",
                                                       function (v)
                                                       {
                                                         staticEnstyle(obj,prop,v);
                                                       }));
                                 }
                                 else if (val instanceof Object)
                                      {
                                        mapWithKeys(val,
                                                    __typedjs("\"flapjax.js\" (line 1410, column 22)",
                                                              function (k,v)
                                                              {
                                                                dynamicEnstyle(obj[prop],k,v);
                                                              }));
                                      }
                                      else {
                                             obj[prop] = val;
                                           };
                               });
var makeTagB = __typedjs("\"flapjax.js\" (line 1421, column 16)",
                         function (tagName)
                         {
                           return __typedjs("\"flapjax.js\" (line 1421, column 43)",
                                            function ()
                                            {
                                              var attribs,children;
                                              if (typeof (arguments[0]) == "object" && ! (arguments[0].nodeType > 0 || arguments[0] instanceof Behavior || arguments[0] instanceof Array))
                                              {
                                                attribs = arguments[0];
                                                children = slice(arguments,1);
                                              }
                                              else {
                                                     attribs = {};
                                                     children = slice(arguments,0);
                                                   };
                                              var elt = document.createElement(tagName);
                                              mapWithKeys(attribs,
                                                          __typedjs("\"flapjax.js\" (line 1437, column 24)",
                                                                    function (name,val)
                                                                    {
                                                                      if (val instanceof Behavior)
                                                                      {
                                                                        elt[name] = val.valueNow();
                                                                        val.liftB(__typedjs("\"flapjax.js\" (line 1440, column 17)",
                                                                                            function (v)
                                                                                            {
                                                                                              staticEnstyle(elt,
                                                                                                            name,
                                                                                                            v);
                                                                                            }));
                                                                      }
                                                                      else {
                                                                             dynamicEnstyle(elt,
                                                                                            name,
                                                                                            val);
                                                                           };
                                                                    }));
                                              deepEach(children,
                                                       __typedjs("\"flapjax.js\" (line 1448, column 22)",
                                                                 function (child)
                                                                 {
                                                                   if (child instanceof Behavior)
                                                                   {
                                                                     var lastVal = child.valueNow();
                                                                     if (lastVal instanceof Array)
                                                                     {
                                                                       lastVal = map(elementize,
                                                                                     lastVal);
                                                                       forEach(__typedjs("\"flapjax.js\" (line 1453, column 17)",
                                                                                         function (dynChild)
                                                                                         {
                                                                                           elt.appendChild(dynChild);
                                                                                         }),
                                                                               lastVal);
                                                                       child.liftB(__typedjs("\"flapjax.js\" (line 1454, column 21)",
                                                                                             function (currentVal)
                                                                                             {
                                                                                               currentVal = map(elementize,
                                                                                                                currentVal);
                                                                                               swapChildren(elt,
                                                                                                            lastVal,
                                                                                                            currentVal);
                                                                                               lastVal = currentVal;
                                                                                             }));
                                                                     }
                                                                     else {
                                                                            lastVal = elementize(lastVal);
                                                                            elt.appendChild(lastVal);
                                                                            child.liftB(__typedjs("\"flapjax.js\" (line 1463, column 21)",
                                                                                                  function (currentVal)
                                                                                                  {
                                                                                                    currentVal = elementize(currentVal);
                                                                                                    elt.replaceChild(currentVal,
                                                                                                                     lastVal);
                                                                                                    lastVal = currentVal;
                                                                                                  }));
                                                                          };
                                                                   }
                                                                   else {
                                                                          elt.appendChild(elementize(child));
                                                                        };
                                                                 }));
                                              return elt;
                                            });
                         });
var generatedTags = ["a",
                     "b",
                     "blockquote",
                     "br",
                     "button",
                     "canvas",
                     "div",
                     "fieldset",
                     "form",
                     "font",
                     "h1",
                     "h2",
                     "h3",
                     "h4",
                     "hr",
                     "img",
                     "iframe",
                     "input",
                     "label",
                     "legend",
                     "li",
                     "ol",
                     "optgroup",
                     "option",
                     "p",
                     "pre",
                     "select",
                     "span",
                     "strong",
                     "table",
                     "tbody",
                     "td",
                     "textarea",
                     "tfoot",
                     "th",
                     "thead",
                     "tr",
                     "tt",
                     "ul"];
forEach(__typedjs("\"flapjax.js\" (line 1488, column 9)",
                  function (tagName)
                  {
                    this[tagName.toUpperCase()] = makeTagB(tagName);
                  }),
        generatedTags);
TEXTB = __typedjs("\"flapjax.js\" (line 1494, column 9)",
                  function (strB)
                  {
                    if (! (strB instanceof Behavior))
                    {
                      strB = constantB(strB);
                    };
                    return startsWith(changes(strB).mapE(__typedjs("\"flapjax.js\" (line 1500, column 7)",
                                                                   function (txt)
                                                                   {
                                                                     return document.createTextNode(txt);
                                                                   })),
                                      document.createTextNode(valueNow(strB)));
                  });
var TEXT = __typedjs("\"flapjax.js\" (line 1504, column 12)",
                     function (str)
                     {
                       return document.createTextNode(str);
                     });
var tagRec = __typedjs("\"flapjax.js\" (line 1515, column 14)",
                       function (eventNames,maker)
                       {
                         if (! (eventNames instanceof Array))
                         {
                           throw "tagRec: expected array of event names as first arg";
                         };
                         if (! (maker instanceof Function))
                         {
                           throw "tagRec: expected function as second arg";
                         };
                         var numEvents = eventNames.length;
                         var receivers = [];
                         var i;
                         for (i = 0; i < numEvents; i ++)
                         {
                           receivers.push(internalE());
                         };
                         var elt = maker.apply(this,receivers);
                         for (i = 0; i < numEvents; i ++)
                         {
                           attachListener(extractEventE(elt,eventNames[i]),receivers[i]);
                         };
                         return elt;
                       });
var extractEventStaticE = __typedjs("\"flapjax.js\" (line 1538, column 27)",
                                    function (domObj,eventName)
                                    {
                                      if (! eventName)
                                      {
                                        throw "extractEventE : no event name specified";
                                      };
                                      if (! domObj)
                                      {
                                        throw "extractEventE : no DOM element specified";
                                      };
                                      domObj = getObj(domObj);
                                      var primEventE = internalE();
                                      addEvent(domObj,
                                               eventName,
                                               __typedjs("\"flapjax.js\" (line 1545, column 29)",
                                                         function (evt)
                                                         {
                                                           sendEvent(primEventE,
                                                                     evt || window.event);
                                                           return true;
                                                         }));
                                      return primEventE;
                                    });
var extractEventE = __typedjs("\"flapjax.js\" (line 1556, column 21)",
                              function (domB,eventName)
                              {
                                if (! (domB instanceof Behavior))
                                {
                                  return extractEventStaticE(domB,eventName);
                                }
                                else {
                                       var domE = domB.changes();
                                       var eventEE = domE.mapE(__typedjs("\"flapjax.js\" (line 1563, column 29)",
                                                                         function (dom)
                                                                         {
                                                                           return extractEventStaticE(dom,
                                                                                                      eventName);
                                                                         }));
                                       var resultE = eventEE.switchE();
                                       sendEvent(domE,domB.valueNow());
                                       return resultE;
                                     };
                                ;;
                              });
var $E = extractEventE;
extractEventsE = __typedjs("\"flapjax.js\" (line 1583, column 18)",
                           function (domObj)
                           {
                             var eventNames = slice(arguments,1);
                             var events = map(__typedjs("\"flapjax.js\" (line 1587, column 5)",
                                                        function (eventName)
                                                        {
                                                          return extractEventE(domObj,eventName);
                                                        }),
                                              eventNames.length === 0 ? [] : eventNames);
                             return mergeE.apply(this,events);
                           });
extractValueOnEventE = __typedjs("\"flapjax.js\" (line 1596, column 24)",
                                 function (triggerE,domObj)
                                 {
                                   if (! (triggerE instanceof EventStream))
                                   {
                                     throw "extractValueOnEventE: expected Event as first arg";
                                   };
                                   return changes(extractValueOnEventB.apply(this,arguments));
                                 });
extractDomFieldOnEventE = __typedjs("\"flapjax.js\" (line 1604, column 27)",
                                    function (triggerE,domObj)
                                    {
                                      if (! (triggerE instanceof EventStream))
                                      {
                                        throw "extractDomFieldOnEventE: expected Event as first arg";
                                      };
                                      var indices = slice(arguments,2);
                                      var res = triggerE.mapE(__typedjs("\"flapjax.js\" (line 1609, column 5)",
                                                                        function ()
                                                                        {
                                                                          return getDomVal(domObj,
                                                                                           indices);
                                                                        }));
                                      return res;
                                    });
var extractValueE = __typedjs("\"flapjax.js\" (line 1613, column 21)",
                              function (domObj)
                              {
                                return changes(extractValueB.apply(this,arguments));
                              });
var extractValueOnEventB = __typedjs("\"flapjax.js\" (line 1619, column 28)",
                                     function (triggerE,domObj)
                                     {
                                       return extractValueStaticB(domObj,triggerE);
                                     });
extractValueStaticB = __typedjs("\"flapjax.js\" (line 1625, column 23)",
                                function (domObj,triggerE)
                                {
                                  var objD;
                                  try
                                  {
                                    objD = getObj(domObj);
                                    if (typeof (domObj) == "string" && objD.id != domObj)
                                    {
                                      throw "Make a radio group";
                                    };
                                  }
                                  catch (e) {
                                              objD = {type: "radio-group", name: domObj};
                                            };
                                  var getter;
                                  switch (objD.type)
                                  {case
                                   "checkbox" :
                                     return startsWith(filterRepeatsE(extractDomFieldOnEventE(triggerE ? triggerE : extractEventsE(objD,
                                                                                                                                   "click",
                                                                                                                                   "keyup",
                                                                                                                                   "change"),
                                                                                              objD,
                                                                                              "checked"),
                                                                      objD.checked),
                                                       objD.checked);
                                   case
                                   "select-one" :
                                     getter = __typedjs("\"flapjax.js\" (line 1659, column 16)",
                                                        function (_)
                                                        {
                                                          return objD.selectedIndex > - 1 ? (objD.options[objD.selectedIndex].value ? objD.options[objD.selectedIndex].value : objD.options[objD.selectedIndex].innerText) : undefined;
                                                        });
                                     return startsWith(filterRepeatsE((triggerE ? triggerE : extractEventsE(objD,
                                                                                                            "click",
                                                                                                            "keyup",
                                                                                                            "change")).mapE(getter)),
                                                       getter(),
                                                       getter());
                                   case
                                   "select-multiple" :
                                     getter = __typedjs("\"flapjax.js\" (line 1678, column 18)",
                                                        function (_)
                                                        {
                                                          var res = [];
                                                          for (var i = 0; i < objD.options.length; i ++)
                                                          {
                                                            if (objD.options[i].selected)
                                                            {
                                                              res.push(objD.options[i].value ? objD.options[i].value : objD.options[i].innerText);
                                                            };
                                                          };
                                                          return res;
                                                        });
                                     return startsWith((triggerE ? triggerE : extractEventsE(objD,
                                                                                             "click",
                                                                                             "keyup",
                                                                                             "change")).mapE(getter),
                                                       getter());
                                   case
                                   "text" :
                                   case
                                   "textarea" :
                                   case
                                   "hidden" :
                                   case
                                   "password" :
                                     return startsWith(filterRepeatsE(extractDomFieldOnEventE(triggerE ? triggerE : extractEventsE(objD,
                                                                                                                                   "click",
                                                                                                                                   "keyup",
                                                                                                                                   "change"),
                                                                                              objD,
                                                                                              "value"),
                                                                      objD.value),
                                                       objD.value);
                                   case
                                   "button" :
                                     return startsWith(extractDomFieldOnEventE(triggerE ? triggerE : extractEventsE(objD,
                                                                                                                    "click",
                                                                                                                    "keyup",
                                                                                                                    "change"),
                                                                               objD,
                                                                               "value"),
                                                       objD.value);
                                   case
                                   "radio" :
                                   case
                                   "radio-group" :
                                     var radiosAD = filter(__typedjs("\"flapjax.js\" (line 1733, column 17)",
                                                                     function (elt)
                                                                     {
                                                                       return (elt.type == "radio") && (elt.getAttribute("name") == objD.name);
                                                                     }),
                                                           document.getElementsByTagName("input"));
                                     getter = objD.type == "radio" ? __typedjs("\"flapjax.js\" (line 1742, column 15)",
                                                                               function (_)
                                                                               {
                                                                                 return objD.checked;
                                                                               }) : __typedjs("\"flapjax.js\" (line 1746, column 15)",
                                                                                              function (_)
                                                                                              {
                                                                                                for (var i = 0; i < radiosAD.length; i ++)
                                                                                                {
                                                                                                  if (radiosAD[i].checked)
                                                                                                  {
                                                                                                    return radiosAD[i].value;
                                                                                                  };
                                                                                                };
                                                                                                return undefined;
                                                                                              });
                                     var actualTriggerE = triggerE ? triggerE : mergeE.apply(this,
                                                                                             map(__typedjs("\"flapjax.js\" (line 1759, column 19)",
                                                                                                           function (radio)
                                                                                                           {
                                                                                                             return extractEventsE(radio,
                                                                                                                                   "click",
                                                                                                                                   "keyup",
                                                                                                                                   "change");
                                                                                                           }),
                                                                                                 radiosAD));
                                     return startsWith(filterRepeatsE(actualTriggerE.mapE(getter),
                                                                      getter()),
                                                       getter());
                                   default:
                                     throw ("extractValueStaticB: unknown value type \"" + objD.type + "\"");};
                                });
var extractValueB = __typedjs("\"flapjax.js\" (line 1776, column 21)",
                              function (domObj)
                              {
                                if (domObj instanceof Behavior)
                                {
                                  return liftB(__typedjs("\"flapjax.js\" (line 1778, column 18)",
                                                         function (dom)
                                                         {
                                                           return extractValueStaticB(dom);
                                                         }),
                                               domObj).switchB();
                                }
                                else {
                                       return extractValueStaticB(domObj);
                                     };
                              });
var $B = extractValueB;
deepStaticUpdate = __typedjs("\"flapjax.js\" (line 1789, column 20)",
                             function (into,from,index)
                             {
                               var fV = (from instanceof Behavior) ? valueNow(from) : from;
                               if (typeof (fV) == "object")
                               {
                                 for (var i in fV)
                                 {
                                   if (! (Object.prototype) || ! (Object.prototype[i]))
                                   {
                                     deepStaticUpdate(index ? into[index] : into,fV[i],i);
                                   };
                                 };
                               }
                               else {
                                      var old = into[index];
                                      into[index] = fV;
                                    };
                             });
deepDynamicUpdate = __typedjs("\"flapjax.js\" (line 1806, column 21)",
                              function (into,from,index)
                              {
                                var fV = (from instanceof Behavior) ? valueNow(from) : from;
                                if (typeof (fV) == "object")
                                {
                                  if (from instanceof Behavior)
                                  {
                                    throw "deepDynamicUpdate: dynamic collections not supported";
                                  };
                                  for (var i in fV)
                                  {
                                    if (! (Object.prototype) || ! (Object.prototype[i]))
                                    {
                                      deepDynamicUpdate(index ? into[index] : into,fV[i],i);
                                    };
                                  };
                                }
                                else {
                                       if (from instanceof Behavior)
                                       {
                                         createNode([changes(from)],
                                                    __typedjs("\"flapjax.js\" (line 1821, column 9)",
                                                              function (p)
                                                              {
                                                                if (index)
                                                                {
                                                                  var old = into[index];
                                                                  into[index] = p.value;
                                                                }
                                                                else {
                                                                       into = p.value;
                                                                     };
                                                                return doNotPropagate;
                                                              }));
                                       };
                                     };
                              });
insertValue = __typedjs("\"flapjax.js\" (line 1834, column 15)",
                        function (val,domObj)
                        {
                          var indices = slice(arguments,2);
                          var parent = getMostDom(domObj,indices);
                          deepStaticUpdate(parent,
                                           val,
                                           indices ? indices[indices.length - 1] : undefined);
                        });
insertValueE = __typedjs("\"flapjax.js\" (line 1841, column 16)",
                         function (triggerE,domObj)
                         {
                           if (! (triggerE instanceof EventStream))
                           {
                             throw "insertValueE: expected Event as first arg";
                           };
                           var indices = slice(arguments,2);
                           var parent = getMostDom(domObj,indices);
                           triggerE.mapE(__typedjs("\"flapjax.js\" (line 1847, column 19)",
                                                   function (v)
                                                   {
                                                     deepStaticUpdate(parent,
                                                                      v,
                                                                      indices ? indices[indices.length - 1] : undefined);
                                                   }));
                         });
insertValueB = __typedjs("\"flapjax.js\" (line 1854, column 16)",
                         function (triggerB,domObj)
                         {
                           var indices = slice(arguments,2);
                           var parent = getMostDom(domObj,indices);
                           deepStaticUpdate(parent,
                                            triggerB,
                                            indices ? indices[indices.length - 1] : undefined);
                           deepDynamicUpdate(parent,
                                             triggerB,
                                             indices ? indices[indices.length - 1] : undefined);
                         });
insertDomE = __typedjs("\"flapjax.js\" (line 1870, column 14)",
                       function (triggerE,domObj)
                       {
                         if (! (triggerE instanceof EventStream))
                         {
                           throw "insertDomE: expected Event as first arg";
                         };
                         var objD = getObj(domObj);
                         var res = triggerE.mapE(__typedjs("\"flapjax.js\" (line 1877, column 5)",
                                                           function (newObj)
                                                           {
                                                             if (! ((typeof (newObj) == "object") && (newObj.nodeType == 1)))
                                                             {
                                                               newObj = module.SPAN({},newObj);
                                                             };
                                                             swapDom(objD,newObj);
                                                             objD = newObj;
                                                             return newObj;
                                                           }));
                         return res;
                       });
insertDomInternal = __typedjs("\"flapjax.js\" (line 1895, column 21)",
                              function (hookD,replaceWithD,optPosition)
                              {
                                switch (optPosition)
                                {case
                                 undefined :
                                 case
                                 null :
                                 case
                                 "over" :
                                   swapDom(hookD,replaceWithD);
                                   break;
                                 case
                                 "before" :
                                   hookD.parentNode.insertBefore(replaceWithD,hookD);
                                   break;
                                 case
                                 "after" :
                                   if (hookD.nextSibling)
                                   {
                                     hookD.parentNode.insertBefore(replaceWithD,hookD.nextSibling);
                                   }
                                   else {
                                          hookD.parentNode.appendChild(replaceWithD);
                                        };
                                   break;
                                 case
                                 "leftMost" :
                                   if (hookD.parentNode.firstChild)
                                   {
                                     hookD.parentNode.insertBefore(replaceWithD,
                                                                   hookD.parentNode.firstChild);
                                   }
                                   else {
                                          hookD.parentNode.appendChild(replaceWithD);
                                        };
                                   break;
                                 case
                                 "rightMost" :
                                   hookD.parentNode.appendChild(replaceWithD);
                                   break;
                                 case
                                 "beginning" :
                                   if (hookD.firstChild)
                                   {
                                     hookD.insertBefore(replaceWithD,hookD.firstChild);
                                   }
                                   else {
                                          hookD.appendChild(replaceWithD);
                                        };
                                   break;
                                 case
                                 "end" :
                                   hookD.appendChild(replaceWithD);
                                   break;
                                 default:
                                   throw ("domInsert: unknown position: " + optPosition);};
                              });
insertDom = __typedjs("\"flapjax.js\" (line 1942, column 13)",
                      function (replaceWithD,hook,optPosition)
                      {
                        insertDomInternal(getObj(hook),
                                          ((typeof (replaceWithD) == "object") && (replaceWithD.nodeType > 0)) ? replaceWithD : document.createTextNode(replaceWithD),
                                          optPosition);
                      });
insertDomB = __typedjs("\"flapjax.js\" (line 1960, column 14)",
                       function (initTriggerB,optID,optPosition)
                       {
                         if (! (initTriggerB instanceof Behavior))
                         {
                           initTriggerB = constantB(initTriggerB);
                         };
                         var triggerB = liftB(__typedjs("\"flapjax.js\" (line 1968, column 5)",
                                                        function (d)
                                                        {
                                                          if ((typeof (d) == "object") && (d.nodeType > 0))
                                                          {
                                                            return d;
                                                          }
                                                          else {
                                                                 var res = document.createElement("span");
                                                                 res.appendChild(document.createTextNode(d));
                                                                 return res;
                                                               };
                                                        }),
                                              initTriggerB);
                         var initD = valueNow(triggerB);
                         if (! ((typeof (initD) == "object") && (initD.nodeType == 1)))
                         {
                           throw ("insertDomB: initial value conversion failed: " + initD);
                         };
                         insertDomInternal(optID === null || optID === undefined ? getObj(initD.getAttribute("id")) : getObj(optID),
                                           initD,
                                           optPosition);
                         var resB = startsWith(insertDomE(changes(triggerB),initD),initD);
                         return resB;
                       });
var extractIdB = __typedjs("\"flapjax.js\" (line 1997, column 18)",
                           function (id,start)
                           {
                             return startsWith(createNode(start instanceof Behavior ? [changes(start)] : [],
                                                          __typedjs("\"flapjax.js\" (line 2002, column 7)",
                                                                    function (p)
                                                                    {
                                                                      p.value = getObj(id);
                                                                      return p;
                                                                    })),
                                               getObj(id));
                           });
var mouseE = __typedjs("\"flapjax.js\" (line 2009, column 14)",
                       function (elem)
                       {
                         return extractEventE(elem,
                                              "mousemove").mapE(__typedjs("\"flapjax.js\" (line 2011, column 9)",
                                                                          function (evt)
                                                                          {
                                                                            if (evt.pageX | evt.pageY)
                                                                            {
                                                                              return {left: evt.pageX, top: evt.pageY};
                                                                            }
                                                                            else if (evt.clientX || evt.clientY)
                                                                                 {
                                                                                   return {left: evt.clientX + document.body.scrollLeft, top: evt.clientY + document.body.scrollTop};
                                                                                 }
                                                                                 else {
                                                                                        return {left: 0, top: 0};
                                                                                      };
                                                                          }));
                       });
var mouseB = __typedjs("\"flapjax.js\" (line 2025, column 14)",
                       function (elem)
                       {
                         return mouseE(elem).startsWith({left: 0, top: 0});
                       });
var mouseLeftB = __typedjs("\"flapjax.js\" (line 2030, column 18)",
                           function (elem)
                           {
                             return liftB(__typedjs("\"flapjax.js\" (line 2031, column 16)",
                                                    function (v)
                                                    {
                                                      return v.left;
                                                    }),
                                          mouseB(elem));
                           });
var mouseTopB = __typedjs("\"flapjax.js\" (line 2035, column 17)",
                          function (elem)
                          {
                            return mouseB(elem).liftB(__typedjs("\"flapjax.js\" (line 2036, column 29)",
                                                                function (v)
                                                                {
                                                                  return v.top;
                                                                }));
                          });
var clicksE = __typedjs("\"flapjax.js\" (line 2041, column 15)",
                        function (elem)
                        {
                          return extractEventE(elem,"click");
                        });
var getURLParam = __typedjs("\"flapjax.js\" (line 2051, column 19)",
                            function (param)
                            {
                              var lparam = param.toLowerCase();
                              var aReturn = [];
                              var strHref = window.location.href;
                              var endstr = (strHref.indexOf("#") > - 1) ? strHref.indexOf("#") : strHref.length;
                              if (strHref.indexOf("?") > - 1)
                              {
                                var strQueryString = strHref.substring(strHref.indexOf("?") + 1,
                                                                       endstr);
                                map(__typedjs("\"flapjax.js\" (line 2058, column 9)",
                                              function (qp)
                                              {
                                                var eq = qp.indexOf("=");
                                                var qname = qp.substr(0,eq + 1).toLowerCase();
                                                if (qname == lparam + "=")
                                                aReturn.push(decodeURIComponent(qp.substr(eq + 1)));
                                              }),
                                    strQueryString.split("&"));
                              };
                              if (aReturn.length == 0)
                              return undefined
                              else if (aReturn.length == 1)
                                   return aReturn[0]
                                   else return aReturn;
                            });
var readCookie = __typedjs("\"flapjax.js\" (line 2072, column 18)",
                           function (name)
                           {
                             var nameEQ = name + "=";
                             var ca = document.cookie.split(";");
                             for (var i = 0; i < ca.length; i ++)
                             {
                               var co = ca[i];
                               while (co.charAt(0) == " ")
                               {
                                 co = co.substring(1,co.length);
                               };
                               if (co.indexOf(nameEQ) === 0)
                               {
                                 return co.substring(nameEQ.length,co.length);
                               };
                             };
                             return undefined;
                           });
var scriptCounter = 0;
var deleteScript = __typedjs("\"flapjax.js\" (line 2088, column 20)",
                             function (scriptID)
                             {
                               var scriptD = getObj(scriptID);
                               scriptD.parentNode.removeChild(scriptD);
                             });
var runScript = __typedjs("\"flapjax.js\" (line 2094, column 17)",
                          function (url,fn,param)
                          {
                            var script = document.createElement("script");
                            script.src = url;
                            var scriptID = "scriptFnRPC" + scriptCounter ++;
                            script.setAttribute("id",scriptID);
                            document.getElementsByTagName("head").item(0).appendChild(script);
                            var timer = {};
                            var check = __typedjs("\"flapjax.js\" (line 2102, column 3)",
                                                  function ()
                                                  {
                                                    eval("try { if (" + param + "!== undefined) {var stat = " + param + ";}} catch (e) {}");
                                                    if (stat !== undefined)
                                                    {
                                                      eval(param + " = undefined;");
                                                      clearInterval(timer.timer);
                                                      clearInterval(timer.timeout);
                                                      if (fn instanceof Function)
                                                      {
                                                        fn(stat);
                                                      };
                                                      deleteScript(scriptID);
                                                    };
                                                  });
                            timer.timer = setInterval(check,3500);
                            timer.timeout = setTimeout(__typedjs("\"flapjax.js\" (line 2117, column 5)",
                                                                 function ()
                                                                 {
                                                                   try
                                                                   {
                                                                     clearInterval(timer.timer);
                                                                   }
                                                                   catch (e) {
                                                                             };
                                                                 }),
                                                       5000);
                          });
var evalForeignScriptValE = __typedjs("\"flapjax.js\" (line 2126, column 29)",
                                      function (urlArgE)
                                      {
                                        var result = receiverE();
                                        urlArgE.mapE(__typedjs("\"flapjax.js\" (line 2128, column 16)",
                                                               function (urlArg)
                                                               {
                                                                 runScript(urlArg.url,
                                                                           __typedjs("\"flapjax.js\" (line 2130, column 9)",
                                                                                     function (val)
                                                                                     {
                                                                                       result.sendEvent(val);
                                                                                     }),
                                                                           urlArg.globalArg);
                                                               }));
                                        return result;
                                      });
var ajaxRequest = __typedjs("\"flapjax.js\" (line 2138, column 19)",
                            function (method,url,body,async,useFlash,callback)
                            {
                              var xhr;
                              if (useFlash)
                              {
                                xhr = new FlashXMLHttpRequest();
                                xhr.onload = __typedjs("\"flapjax.js\" (line 2142, column 18)",
                                                       function ()
                                                       {
                                                         callback(xhr);
                                                       });
                              }
                              else if (window.XMLHttpRequest && ! (window.ActiveXObject))
                                   {
                                     xhr = new window.XMLHttpRequest();
                                     xhr.onload = __typedjs("\"flapjax.js\" (line 2146, column 18)",
                                                            function ()
                                                            {
                                                              callback(xhr);
                                                            });
                                   }
                                   else if (window.ActiveXObject)
                                        {
                                          try
                                          {
                                            xhr = new ActiveXObject("Msxml2.XMLHTTP");
                                          }
                                          catch (e) {
                                                      xhr = new ActiveXObject("Microsoft.XMLHTTP");
                                                    };
                                          xhr.onreadystatechange = __typedjs("\"flapjax.js\" (line 2152, column 30)",
                                                                             function ()
                                                                             {
                                                                               if (xhr.readyState == 4)
                                                                               {
                                                                                 callback(xhr);
                                                                               };
                                                                             });
                                        };
                              xhr.open(method,url,async);
                              if (method == "POST")
                              {
                                xhr.setRequestHeader("Content-Type",
                                                     "application/x-www-form-urlencoded");
                              };
                              xhr.send(body);
                              return xhr;
                            });
var encodeREST = __typedjs("\"flapjax.js\" (line 2167, column 18)",
                           function (obj)
                           {
                             var str = "";
                             for (var field in obj)
                             {
                               if (typeof (obj[field]) !== "function")
                               {
                                 if (str != "")
                                 {
                                   str += "&";
                                 };
                                 str += field + "=" + encodeURIComponent(obj[field]);
                               };
                             };
                             return str;
                           });
var parseJSON = __typedjs("\"flapjax.js\" (line 2179, column 17)",
                          function (str)
                          {
                            try
                            {
                              return ! (/[^,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t]/.test(str.replace(/"(\\.|[^"\\])*"/g,
                                                                                              ""))) && eval("(" + str + ")");
                            }
                            catch (e) {
                                        throw "cannot parse JSON string: " + e;
                                      };
                          });
var toJSONString = (__typedjs("\"flapjax.js\" (line 2189, column 21)",
                              function ()
                              {
                                var m = {"\b": "\\b", "\t": "\\t", "\n": "\\n", "\f": "\\f", "\r": "\\r", "\"": "\\\"", "\\": "\\\\"};
                                var s = {array: __typedjs("\"flapjax.js\" (line 2200, column 12)",
                                                          function (x)
                                                          {
                                                            var a = ["["],b,f,i,l = x.length,v;
                                                            for (i = 0; i < l; i += 1)
                                                            {
                                                              v = x[i];
                                                              f = s[typeof v];
                                                              if (f)
                                                              {
                                                                v = f(v);
                                                                if (typeof v == "string")
                                                                {
                                                                  if (b)
                                                                  {
                                                                    a[a.length] = ",";
                                                                  };
                                                                  a[a.length] = v;
                                                                  b = true;
                                                                };
                                                              };
                                                            };
                                                            a[a.length] = "]";
                                                            return a.join("");
                                                          }), "boolean": __typedjs("\"flapjax.js\" (line 2219, column 16)",
                                                                                   function (x)
                                                                                   {
                                                                                     return String(x);
                                                                                   }), "null": __typedjs("\"flapjax.js\" (line 2222, column 13)",
                                                                                                         function (x)
                                                                                                         {
                                                                                                           return "null";
                                                                                                         }), number: __typedjs("\"flapjax.js\" (line 2225, column 13)",
                                                                                                                               function (x)
                                                                                                                               {
                                                                                                                                 return isFinite(x) ? String(x) : "null";
                                                                                                                               }), object: __typedjs("\"flapjax.js\" (line 2228, column 13)",
                                                                                                                                                     function (x)
                                                                                                                                                     {
                                                                                                                                                       if (x)
                                                                                                                                                       {
                                                                                                                                                         if (x instanceof Array)
                                                                                                                                                         {
                                                                                                                                                           return s.array(x);
                                                                                                                                                         };
                                                                                                                                                         var a = ["{"],
                                                                                                                                                             b,
                                                                                                                                                             f,
                                                                                                                                                             i,
                                                                                                                                                             v;
                                                                                                                                                         for (i in x)
                                                                                                                                                         {
                                                                                                                                                           v = x[i];
                                                                                                                                                           f = s[typeof v];
                                                                                                                                                           if (f)
                                                                                                                                                           {
                                                                                                                                                             v = f(v);
                                                                                                                                                             if (typeof v == "string")
                                                                                                                                                             {
                                                                                                                                                               if (b)
                                                                                                                                                               {
                                                                                                                                                                 a[a.length] = ",";
                                                                                                                                                               };
                                                                                                                                                               a.push(s.string(i),
                                                                                                                                                                      ":",
                                                                                                                                                                      v);
                                                                                                                                                               b = true;
                                                                                                                                                             };
                                                                                                                                                           };
                                                                                                                                                         };
                                                                                                                                                         a[a.length] = "}";
                                                                                                                                                         return a.join("");
                                                                                                                                                       };
                                                                                                                                                       return "null";
                                                                                                                                                     }), string: __typedjs("\"flapjax.js\" (line 2253, column 13)",
                                                                                                                                                                           function (x)
                                                                                                                                                                           {
                                                                                                                                                                             if (/["\\\x00-\x1f]/.test(x))
                                                                                                                                                                             {
                                                                                                                                                                               x = x.replace(/([\x00-\x1f\\"])/g,
                                                                                                                                                                                             __typedjs("\"flapjax.js\" (line 2255, column 44)",
                                                                                                                                                                                                       function (a,
                                                                                                                                                                                                                 b)
                                                                                                                                                                                                       {
                                                                                                                                                                                                         var c = m[b];
                                                                                                                                                                                                         if (c)
                                                                                                                                                                                                         {
                                                                                                                                                                                                           return c;
                                                                                                                                                                                                         };
                                                                                                                                                                                                         c = b.charCodeAt();
                                                                                                                                                                                                         return "\\u00" + Math.floor(c / 16).toString(16) + (c % 16).toString(16);
                                                                                                                                                                                                       }));
                                                                                                                                                                             };
                                                                                                                                                                             return "\"" + x + "\"";
                                                                                                                                                                           })};
                                return __typedjs("\"flapjax.js\" (line 2269, column 10)",
                                                 function (val)
                                                 {
                                                   var f = s[typeof val];
                                                   if (f)
                                                   {
                                                     return f(val);
                                                   }
                                                   else {
                                                          throw "parseJSON: unknown object type: " + (typeof val);
                                                        };
                                                 });
                              }))();
var serverRequestE = __typedjs("\"flapjax.js\" (line 2276, column 22)",
                               function (useFlash,requestE)
                               {
                                 var responseE = receiverE();
                                 requestE.mapE(__typedjs("\"flapjax.js\" (line 2279, column 17)",
                                                         function (obj)
                                                         {
                                                           var body = "";
                                                           var method = "GET";
                                                           var url = obj.url;
                                                           var reqType = obj.request ? obj.request : (obj.fields ? "post" : "get");
                                                           if (obj.request == "get")
                                                           {
                                                             url += "?" + encodeREST(obj.fields);
                                                             body = "";
                                                             method = "GET";
                                                           }
                                                           else if (obj.request == "post")
                                                                {
                                                                  body = toJSONString(obj.fields);
                                                                  method = "POST";
                                                                }
                                                                else if (obj.request == "rawPost")
                                                                     {
                                                                       body = obj.body;
                                                                       method = "POST";
                                                                     }
                                                                     else if (obj.request == "rest")
                                                                          {
                                                                            body = encodeREST(obj.fields);
                                                                            method = "POST";
                                                                          }
                                                                          else {
                                                                                 throw ("Invalid request type: " + obj.request);
                                                                               };
                                                           var async = obj.async;
                                                           var xhr;
                                                           if (obj.response == "json")
                                                           {
                                                             xhr = ajaxRequest(method,
                                                                               url,
                                                                               body,
                                                                               async,
                                                                               useFlash,
                                                                               __typedjs("\"flapjax.js\" (line 2311, column 11)",
                                                                                         function (xhr)
                                                                                         {
                                                                                           responseE.sendEvent(parseJSON(xhr.responseText));
                                                                                         }));
                                                           }
                                                           else if (obj.response == "xml")
                                                                {
                                                                  ajaxRequest(method,
                                                                              url,
                                                                              body,
                                                                              async,
                                                                              useFlash,
                                                                              __typedjs("\"flapjax.js\" (line 2317, column 11)",
                                                                                        function (xhr)
                                                                                        {
                                                                                          responseE.sendEvent(xhr.responseXML);
                                                                                        }));
                                                                }
                                                                else if (obj.response == "plain" || ! obj.response)
                                                                     {
                                                                       ajaxRequest(method,
                                                                                   url,
                                                                                   body,
                                                                                   async,
                                                                                   useFlash,
                                                                                   __typedjs("\"flapjax.js\" (line 2323, column 11)",
                                                                                             function (xhr)
                                                                                             {
                                                                                               responseE.sendEvent(xhr.responseText);
                                                                                             }));
                                                                     }
                                                                     else {
                                                                            throw ("Unknown response format: " + obj.response);
                                                                          };
                                                         }));
                                 return responseE;
                               });
var getWebServiceObjectE = __typedjs("\"flapjax.js\" (line 2335, column 28)",
                                     function (requestE)
                                     {
                                       return serverRequestE(false,requestE);
                                     });
var getForeignWebServiceObjectE = __typedjs("\"flapjax.js\" (line 2340, column 35)",
                                            function (requestE)
                                            {
                                              return serverRequestE(true,requestE);
                                            });
var cumulativeOffset = __typedjs("\"flapjax.js\" (line 2345, column 24)",
                                 function (element)
                                 {
                                   var valueT = 0,valueL = 0;
                                   do
                                   {
                                     valueT += element.offsetTop || 0;
                                     valueL += element.offsetLeft || 0;
                                     element = element.offsetParent;
                                   } while (element);
                                   return {left: valueL, top: valueT};
                                 });
var mixedSwitchB = __typedjs("\"flapjax.js\" (line 2372, column 20)",
                             function (behaviourCreatorsB)
                             {
                               var init = valueNow(behaviourCreatorsB);
                               var prevSourceE = null;
                               var receiverE = internalE();
                               var makerE = createNode([changes(behaviourCreatorsB)],
                                                       __typedjs("\"flapjax.js\" (line 2383, column 5)",
                                                                 function (p)
                                                                 {
                                                                   if (prevSourceE != null)
                                                                   {
                                                                     removeListener(prevSourceE,
                                                                                    receiverE);
                                                                     prevSourceE = null;
                                                                   };
                                                                   if (p.value instanceof Behavior)
                                                                   {
                                                                     prevSourceE = changes(p.value);
                                                                     attachListener(prevSourceE,
                                                                                    receiverE);
                                                                     sendEvent(receiverE,
                                                                               valueNow(p.value));
                                                                   }
                                                                   else {
                                                                          sendEvent(receiverE,
                                                                                    p.value);
                                                                        };
                                                                 }));
                               if (init instanceof Behavior)
                               {
                                 sendEvent(makerE,init);
                               };
                               return startsWith(receiverE,
                                                 init instanceof Behavior ? valueNow(init) : init);
                             });
var compilerInsertDomB = __typedjs("\"flapjax.js\" (line 2407, column 26)",
                                   function (mixedB,target)
                                   {
                                     insertDomB(mixedSwitchB(mixedB),target,"over");
                                   });
var compilerInsertValueB = __typedjs("\"flapjax.js\" (line 2410, column 28)",
                                     function (mixedB,target,attrib)
                                     {
                                       console.log(mixedB);
                                       if (typeof (mixedB) == "object")
                                       {
                                         for (var ix in mixedB)
                                         {
                                           if (Object.prototype && Object.prototype[ix])
                                           {
                                             continue;
                                             ;;
                                           };
                                           if (mixedB[ix] instanceof Behavior)
                                           {
                                             insertValueB(mixedSwitchB(mixedB[ix]),
                                                          target,
                                                          attrib,
                                                          ix);
                                           }
                                           else {
                                                  insertValueB(constantB(mixedB[ix]),
                                                               target,
                                                               attrib,
                                                               ix);
                                                };
                                         };
                                         ;;
                                       }
                                       else {
                                              insertValueB(mixedSwitchB(mixedB),target,attrib);
                                            };
                                     });
var compilerLift = __typedjs("\"flapjax.js\" (line 2425, column 20)",
                             function (f)
                             {
                               checkBehavior:
                               {
                                 for (var i = 0; i < arguments.length; i ++)
                                 {
                                   if (arguments[i] instanceof Behavior)
                                   {
                                     break checkBehavior;
                                   };
                                 };
                                 return f.apply(this,slice(arguments,1));
                               };
                               var resultE = internalE();
                               var r = liftB.apply(this,arguments);
                               if (! (r instanceof Behavior))
                               {
                                 return r;
                               };
                               if (r.valueNow() instanceof EventStream)
                               {
                                 return r.valueNow();
                               }
                               else {
                                      return mixedSwitchB(r);
                                    };
                             });
var compilerCall = __typedjs("\"flapjax.js\" (line 2448, column 20)",
                             function (f)
                             {
                               return compilerLift.apply(this,arguments);
                             });
var compilerIf = __typedjs("\"flapjax.js\" (line 2453, column 18)",
                           function (test,cons,alt)
                           {
                             if (test instanceof Behavior)
                             {
                               return test.liftB(__typedjs("\"flapjax.js\" (line 2455, column 23)",
                                                           function (v)
                                                           {
                                                             return v ? cons : alt;
                                                           })).switchB();
                             }
                             else {
                                    return test ? cons : alt;
                                  };
                           });
var unBehavior = __typedjs("\"flapjax.js\" (line 2463, column 18)",
                           function (recompute)
                           {
                             return __typedjs("\"flapjax.js\" (line 2463, column 47)",
                                              function (v)
                                              {
                                                if (v instanceof Behavior)
                                                {
                                                  if (v.valueNow() instanceof Behavior)
                                                  {
                                                    return unBehavior(recompute)(v.valueNow());
                                                  }
                                                  else {
                                                         attachListener(v.changes(),
                                                                        recompute(v.changes()));
                                                         return unBehavior()(v.valueNow());
                                                       };
                                                }
                                                else if (typeof v == "function")
                                                     {
                                                       return __typedjs("\"flapjax.js\" (line 2474, column 12)",
                                                                        function ()
                                                                        {
                                                                          var r = v.apply(this,
                                                                                          arguments);
                                                                          return unBehavior(recompute)(r);
                                                                        });
                                                     }
                                                     else {
                                                            return v;
                                                          };
                                                ;;
                                              });
                           });
var compilerEventStreamArg = __typedjs("\"flapjax.js\" (line 2485, column 30)",
                                       function (x)
                                       {
                                         if (x instanceof Behavior)
                                         {
                                           return compilerEventStreamArg(x.valueNow());
                                         }
                                         else if (typeof (x) == "function")
                                              {
                                                return __typedjs("\"flapjax.js\" (line 2489, column 12)",
                                                                 function ()
                                                                 {
                                                                   return compilerEventStreamArg(x.apply(this,
                                                                                                         arguments));
                                                                 });
                                              }
                                              else {
                                                     return x;
                                                   };
                                       });
var compilerUnbehavior = __typedjs("\"flapjax.js\" (line 2495, column 26)",
                                   function (v)
                                   {
                                     if (typeof v == "function")
                                     {
                                       return __typedjs("\"flapjax.js\" (line 2497, column 12)",
                                                        function ()
                                                        {
                                                          var originalArgs = slice(arguments,0);
                                                          var srcs = [];
                                                          var recompute = __typedjs("\"flapjax.js\" (line 2503, column 23)",
                                                                                    function (src)
                                                                                    {
                                                                                      srcs.push(src);
                                                                                      return recomputeE;
                                                                                    });
                                                          var resultE = internalE();
                                                          var recomputeE = createNode([],
                                                                                      __typedjs("\"flapjax.js\" (line 2510, column 38)",
                                                                                                function (send,
                                                                                                          _)
                                                                                                {
                                                                                                  map1(__typedjs("\"flapjax.js\" (line 2513, column 14)",
                                                                                                                 function (src)
                                                                                                                 {
                                                                                                                   removeListener(src,
                                                                                                                                  recomputeE);
                                                                                                                 }),
                                                                                                       srcs);
                                                                                                  srcs = [];
                                                                                                  var args = map1(unBehavior(recompute),
                                                                                                                  originalArgs);
                                                                                                  var r = v.apply(this,
                                                                                                                  args);
                                                                                                  sendEvent(resultE,
                                                                                                            r);
                                                                                                }));
                                                          return resultE.startsWith(v.apply(this,
                                                                                            map1(unBehavior(recompute),
                                                                                                 originalArgs)));
                                                        });
                                     }
                                     else {
                                            return v;
                                          };
                                   });
