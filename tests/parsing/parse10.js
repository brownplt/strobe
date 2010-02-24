var zip = function(arrays) {
  if (arrays.length == 0) return [];
  var ret = [];
  for(var i=0; i<arrays[0].length;i++) {
    ret.push([]);
    for(var j=0; j<arrays.length;j++) 
      ret[i].push(arrays[j][i]);
  }
  return ret;
};
