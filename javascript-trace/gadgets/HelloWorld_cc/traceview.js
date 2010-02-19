var refs = detailsViewData.getValue("refs");
var __typedJsTypes = refs.__typedJsTypes;
var __orderedVars = refs.__orderedVars;
var data = refs.win.data;

function updateView() {
  var res = "";
  for (var i=0; i < data.length; i++) {
    res += data[i] + "\n";
  }
  display.value = res;
}

updateView();
setInterval(updateView, 1000);
