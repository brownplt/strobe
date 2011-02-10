ADSAFE.lib("test",
           function() {
               var o = {};
               o.foo = o.__proto__;
           }
          );