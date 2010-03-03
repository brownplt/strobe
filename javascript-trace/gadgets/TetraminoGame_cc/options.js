var keyCodeLeft = 0;
var keyCodeRight = 0;
var keyCodeRotate = 0;
var keyCodeDown = 0;
var keyCodeFall = 0;

function view_onOpen() {

  keyLeft.value = options.getValue("keyLeftHR");
  keyRight.value = options.getValue("keyRightHR");
  keyRotate.value = options.getValue("keyRotateHR");
  keyDown.value = options.getValue("keyDownHR");
  keyFall.value = options.getValue("keyFallHR");

  keyCodeLeft = options.getValue("keyLeft");
  keyCodeRight = options.getValue("keyRight");
  keyCodeRotate = options.getValue("keyRotate");
  keyCodeDown = options.getValue("keyDown");
  keyCodeFall = options.getValue("keyFall");

  optMusic.value = options.getValue("tetrisMusic");
}

function keyLeftCode() {
  gadget.debug.trace("New Key Left: "+event.keyCode);
  var codes = new keyCodes();
  keyCodeLeft = event.keyCode;
  gadget.debug.trace("New KeyCode: "+codes.getNameForKey( event.keyCode ));
  keyLeft.value = codes.getNameForKey( event.keyCode );
  event.returnValue = false;
}

function keyRightCode() {
  gadget.debug.trace("New Key Right: "+event.keyCode);
  var codes = new keyCodes();
  keyCodeRight = event.keyCode;
  gadget.debug.trace("New KeyCode: "+codes.getNameForKey( event.keyCode ));
  keyRight.value = codes.getNameForKey( event.keyCode );
  event.returnValue = false;
}

function keyRotateCode() {
  gadget.debug.trace("New Key Rotate: "+event.keyCode);
  var codes = new keyCodes();
  keyCodeRotate = event.keyCode;
  gadget.debug.trace("New KeyCode: "+codes.getNameForKey( event.keyCode ));
  keyRotate.value = codes.getNameForKey( event.keyCode );
  event.returnValue = false;
}

function keyDownCode() {
  gadget.debug.trace("New Key Down: "+event.keyCode);
  var codes = new keyCodes();
  keyCodeDown = event.keyCode;
  gadget.debug.trace("New KeyCode: "+codes.getNameForKey( event.keyCode ));
  keyDown.value = codes.getNameForKey( event.keyCode );
  event.returnValue = false;
}

function keyFallCode() {
  gadget.debug.trace("New Key Fall: "+event.keyCode);
  var codes = new keyCodes();
  keyCodeFall = event.keyCode;
  gadget.debug.trace("New KeyCode: "+codes.getNameForKey( event.keyCode ));
  keyFall.value = codes.getNameForKey( event.keyCode );
  event.returnValue = false;
}

function optionsSave() {
  gadget.debug.trace("Saving options");

  options.putValue("keyLeftHR", keyLeft.value);
  options.putValue("keyLeft", keyCodeLeft);
  options.putValue("keyRightHR", keyRight.value);
  options.putValue("keyRight", keyCodeRight);
  options.putValue("keyRotateHR", keyRotate.value);
  options.putValue("keyRotate", keyCodeRotate);
  options.putValue("keyDownHR", keyDown.value);
  options.putValue("keyDown", keyCodeDown);
  options.putValue("keyFallHR", keyFall.value);
  options.putValue("keyFall", keyCodeFall);
  options.putValue("tetrisMusic", optMusic.value);
}
