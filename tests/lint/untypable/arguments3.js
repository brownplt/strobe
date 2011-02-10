ADSAFE.lib("test",
           function () {
               function f() {}
               function g() {
                   f.arguments[0] = 12;
               };
           });

