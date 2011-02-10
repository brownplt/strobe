ADSAFE.lib("test",
           function() {
               function f() {
                   f.toString.apply(null);
               }
           }
          );