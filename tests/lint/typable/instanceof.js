ADSAFE.lib("TEST_",
           function () {
               function foo() {}
               var o = {};
               if(o instanceof foo) {
                   o.boozle();
               }
           });
