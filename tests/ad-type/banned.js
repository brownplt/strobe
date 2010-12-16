var banned = {};
function error() /*: -> Bot */ { throw 0; }

var reject_name = 
    /*: cheat (('banned -> True) + ('not_banned -> False)) */
(function (name) /*: 'Ad -> Bool */ {
    return 
    ((typeof name !== 'number' || name < 0) &&
     (typeof name !== 'string' || name.charAt(0) === '_' ||
      name.slice(-1) === '_' || name.charAt(0) === '-'))
        || banned[name];
});


function get(object, name) 
/*: 'Ad * 'Ad -> 'Ad */ 
{
    if (typeof object !== "object") { return error(); }
    if (!reject_name(name)) {
        return object[name];
    }
    return error();
}
