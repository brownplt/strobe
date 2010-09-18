//  The reject function enforces the restriction on get and put.
//  It allows access only to objects and arrays. It does not allow use of
//  the banned names, or names that are not strings or positive numbers,
//  or strings that start or end with _ or strings that start with -.

function reject(object, name) 
/*: 'Ad * 'Ad -> Any */ 
{
    return typeof object !== 'object'  || /*: cheat Any */ (banned[name]) ||
        ((typeof name !== 'number' || name < 0) &&
         (typeof name !== 'string'  || /*: cheat Str */ name.charAt(0) === '_' ||
          /*: cheat Str */ name.slice(-1) === '_'     || /*: cheat Str */ name.charAt(0) === '-'));
}

function ADSAFE_get(object, name) 
/*: 'Ad * 'Ad -> 'Ad */ 
{
    if (/* arguments.length  === 2  && */!reject(object, name)) {
        return /*: cheat 'Ad */ (object[/*: cheat Str */name]);
    }
    return error();
}

//  ADSAFE.remove deletes a value from an object.

function ADSAFE_remove (object, name) 
/*: 'Ad * 'Ad -> 'Ad */
{
    if (/* arguments.length === 2  && */ !reject(object, name)) {
        /*: cheat Bool */ (delete object[/*: cheat Str + Int */ name]);
        return;
    }
    return error();
}

//  ADSAFE.set stores a value in an object.

function ADSAFE_set (object, name, value) 
/*: 'Ad * 'Ad * 'Ad -> 'Ad */
{
    if (/* arguments.length === 3 && */ !reject(object, name)) {
        /*: cheat 'Ad */ (object[/*: cheat Str */ name] = value);
        return;
    }
    return error();
}
