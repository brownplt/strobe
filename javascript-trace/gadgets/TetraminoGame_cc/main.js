var tetris = null;

function view_onOpen() {

    options.putDefaultValue("keyLeftHR", "Left");
    options.putDefaultValue("keyLeft", 37);
    options.putDefaultValue("keyRightHR", "Right");
    options.putDefaultValue("keyRight", 39);
    options.putDefaultValue("keyRotateHR", "Up");
    options.putDefaultValue("keyRotate", 38);
    options.putDefaultValue("keyDownHR", "Down");
    options.putDefaultValue("keyDown", 40);
    options.putDefaultValue("keyFallHR", "Space");
    options.putDefaultValue("keyFall", 32);
    options.putDefaultValue("tetrisMusic", true);

    plugin.onAddCustomMenuItems = AddCustomMenuItems;

    for (x = 0; x < 10; x++) {
        for (y = 0; y < 16; y++) {
            divBackground.appendElement("<div name=\"s" + y + "_" + x + "\" x=\"" + (x * 15) + "\" y=\"" + (y * 15) + "\" height=\"15\" width=\"15\" background=\"#FFC000\" />");
        }
    }
    divBackground.focus();
    init();
    labelStatus.visible = false;
    btnStart.visible = true;
}

// Adds our plugin specific items to the menu


function AddCustomMenuItems(menu) {
    menu.AddItem(strMenuNew, 0, OnMenuClicked);
}

function OnMenuClicked(itemText) {
    if (itemText == strMenuNew) {
        gameStart();
    }
}

function gameLeave() {
    pause();
    labelStatus.innerText = strPaused;
    labelStatus.visible = true;
}

function gameFocus() {
    if (gamePaused) {
        resume();
        labelStatus.visible = false;
    }
}

function gameStart() {
    init();
    start();
    btnStart.visible = false;
    labelStatus.visible = false;
    divBackground.focus();
}

function gameKeyDown() {
    if (!gameStarted || gamePaused) return;
    //gadget.debug.trace("KeyDown: "+event.keyCode);
    switch (event.keyCode) {
    case options.getValue("keyLeft"):
        moveleft();
        break;
    case options.getValue("keyRight"):
        moveright();
        break;
    case options.getValue("keyRotate"):
        rotate();
        break;
    case options.getValue("keyDown"):
        movedown();
        break;
    case options.getValue("keyFall"):
        fall();
        break;
    }
}

function gameKeyUp() {
    //gadget.debug.trace("KeyUp: "+event.keyCode);
}