
var o = {f: function(g) /*: (-> (-> Undef)) -> Undef */ { }}; 

o.f(function() /*: -> (-> Undef) */ {
    return function () /*: -> Undef */ {
        function bar(thing) /*: Any -> Undef */ {
            
        }
        bar("anything");
    };
});
