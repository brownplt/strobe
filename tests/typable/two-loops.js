function foo(arr, to_add) /*: Array<Int> * Array<Int> + Int -> Array<Int> */ {

    var i /*: upcast Undef + Int */;

    if(to_add instanceof Array) {
        for(i = 0; i < arr.length; i += 1) {
            arr[i] += to_add[i];
            while(arr[i] > 10) {
                arr[i] -= 1;
            }
        }
    }
    else {
        for(i = 0; i < arr.length; i += 1) {
            arr[i] += to_add;
        }
    }
    return arr;
}