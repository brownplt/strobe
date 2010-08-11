function reject(object, name) {
    return typeof object !== 'object'  || banned[name] ||
        ((typeof name !== 'number' || name < 0) &&
         (typeof name !== 'string'  || name.charAt(0) === '_' ||
          name.slice(-1) === '_'     || name.charAt(0) === '-'));
}

function ADSAFE_get(object, name) 
/*: Ad * Ad -> Ad */ {
    if (arguments.length === 2 && !reject(object, name)) {
        return object[name];
    }
    return error();
}