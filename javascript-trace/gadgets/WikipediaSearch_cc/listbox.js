var AREA_IMAGE = "images/results_default.png";
var AREA_LEFT_BORDER = 3;
var AREA_TOP_BORDER = 3;
var AREA_BOTTOM_BORDER = 3;
var AREA_RIGHT_BORDER = 3;
var ITEM_HEIGHT = 15;
var ITEM_TEXT_SIZE = 9;
var ITEM_TEXT_FONT = "Geneva";
var ITEM_SELECTOR_COLOR = "#000000";
var ITEM_SELECTOR_OPACITY = "50";
var ITEM_HOVER_SELECTOR_COLOR = "#000000";
var ITEM_HOVER_SELECTOR_OPACITY = "20";

// This class creates and shows a ListBox which can be used to display multiple
// items. Scrolling is implemented too so feel free to overpopulate the list.
function ListBox(parentDivElement_, acceptedCallback_, cancelledCallback_) {
  var shown_ = false;
  var divElement_ = null;
  var backgroundElement_ = null;
  var selectorElement_ = null;
  var hoverElement_ = null;
  var items_ = [];
  var selectedItem_ = null;
  var hoveredItem_ = null;
  var firstVisibleItem_ = null;
  var totalVisibleItems_ = null;

  // Add an item to the end of the listbox
  function addItem(displayValue) {
    var newItem = new Object();
    newItem.displayValue = displayValue;
    newItem.labelElement = null;

    items_.push(newItem);
  }

  function show(selectItem) {
    shown_ = true;

    // If a background image was specified, draw it at the top left
    backgroundElement_ = parentDivElement_.appendElement("<img " +
      "src=\"" + AREA_IMAGE + "\" " +
      "width=\"" + parentDivElement_.width + "\" height=\"" +
      parentDivElement_.height + "\" x=\"0\" y=\"0\" />");

    // Create the main item which will contain the items
    divElement_ = parentDivElement_.appendElement("<div " +
      "x=\"" + AREA_LEFT_BORDER + "\" y=\"" + AREA_TOP_BORDER + "\" " +
      "width=\"" + (parentDivElement_.width - AREA_RIGHT_BORDER - AREA_LEFT_BORDER - 1) + "\" " +
      "height=\"" + (parentDivElement_.height - AREA_BOTTOM_BORDER - AREA_TOP_BORDER - 1) + "\" />");

    // Create the selector picture
    selectorElement_ = divElement_.appendElement("<div " +
      "width=\"" + divElement_.width + "\" " +
      "height=\"" + ITEM_HEIGHT + "\" background=\"" + ITEM_SELECTOR_COLOR + "\" " +
      "opacity=\"" + ITEM_SELECTOR_OPACITY + "\" visible=\"false\" />");

    // Create the hovered selector picture
    hoverElement_ = divElement_.appendElement("<div " +
      "width=\"" + divElement_.width + "\" " +
      "height=\"" + ITEM_HEIGHT + "\" background=\"" + ITEM_HOVER_SELECTOR_COLOR + "\" " +
      "opacity=\"" + ITEM_HOVER_SELECTOR_OPACITY + "\" visible=\"false\" />");

    // Draw the current selection and items
    if (selectItem != null)
      selectedItem_ = selectItem;

    hoveredItem_ = null;
    firstVisibleItem_ = 0;
    totalVisibleItems_ = parseInt(divElement_.height / ITEM_HEIGHT);

    showSelectedItem();
    drawAllVisibleItems();
  }

  // Removes all elements in the main div and resets the state
  function hide() {
    if (!shown_)
      return;
    shown_ = false;

    parentDivElement_.removeElement(backgroundElement_);
    parentDivElement_.removeElement(divElement_);

    items_ = []
  }

  function drawAllVisibleItems() {
    for (var i = 0; i <= items_.length - 1; i++) {
      // If the item exists, remove it
      if (items_[i].labelElement != null)
        divElement_.removeElement(items_[i].labelElement);

      // Draw the item in the proper position
      var drawY = getYPositionOfItem(i);
      var cleanText = items_[i].displayValue.replace(/\&/g, " ");

      if (drawY != null){
        items_[i].labelElement = divElement_.appendElement("<label " +
          "x=\"" + getXPositionOfItem(i) + "\" " +
          "y=\"" + drawY + "\" " +
          "width=\"" + divElement_.width + "\" " +
          "height=\"" + ITEM_HEIGHT + "\" " +
          "size=\"" + ITEM_TEXT_SIZE + "\" font=\"" + ITEM_TEXT_FONT + "\" " +
          "trimming=\"character-ellipsis\" enabled=\"true\">" +
          cleanText + "</label>");
        setMouseEventsForLabel(items_[i].labelElement, i);
      }
    }
  }

  // Used to set the mouse events for a certain item element
  function setMouseEventsForLabel(element, index) {
    element.onmouseover = function () { onMouseOverItem(index); };
    element.onmouseout = function () { onMouseOffItem(index); };
    element.onclick = function () { onMouseClickItem(index); };
  }

  // Called when the mouse goes over an item
  function onMouseOverItem(index) {
    hoveredItem_ = index;
    showHoveredItem();
  }

  // Called when the mouse leaves an item
  function onMouseOffItem(index) {
    hoveredItem_ = null;
    hideHoveredItem();
  }

  // Called when the user clicks on an item
  function onMouseClickItem(index) {
    acceptedCallback_(items_[index].displayValue);
  }

  // Returns the y position of an item (if visible)
  function getYPositionOfItem(index) {
    var indexPosition = index - firstVisibleItem_;

    if ((indexPosition < 0) || (indexPosition >= totalVisibleItems_))
      return null;

    return indexPosition * ITEM_HEIGHT;
  }

  // Returns the x position of an item
  function getXPositionOfItem(index) {
    return 0;
  }

  function showSelectedItem() {
    if (selectedItem_ == null)
      return;

    // Verify bounds
    if (selectedItem_ < 0)
      selectedItem_ = 0;
    if (selectedItem_ > items_.length - 1)
      selectedItem_ = items_.length - 1;

    // Check if scroll up is needed
    if (selectedItem_ < firstVisibleItem_) {
      firstVisibleItem_ = selectedItem_;
      drawAllVisibleItems();
    }

    // Check if scroll down is needed
    if (selectedItem_ > firstVisibleItem_ + totalVisibleItems_ - 1) {
      firstVisibleItem_ = selectedItem_ - totalVisibleItems_ + 1;
      drawAllVisibleItems();
    }

    // Select the item
    selectorElement_.x = 0;
    selectorElement_.y = getYPositionOfItem(selectedItem_);
    selectorElement_.visible = true;
  }

  function showHoveredItem() {
    // Place the hovered selector
    hoverElement_.x = 0;
    hoverElement_.y = getYPositionOfItem(hoveredItem_);
    hoverElement_.visible = true;
  }

  function hideHoveredItem() {
    // Place the hovered selector
    hoverElement_.visible = false;
  }

  // Does the action associated with a certain key
  function notifyKeyPress(keyCode) {
    if (!shown_)
      return false;

    if (keyCode == 38) {            // Up arrow
      selectedItem_--;
      showSelectedItem();

    } else if (keyCode == 40) {     // Down arrow
      selectedItem_++;
      showSelectedItem();

    } else if (keyCode == 33) {     // Page up
      selectedItem_ -= totalVisibleItems_ - 1;
      showSelectedItem();

    } else if (keyCode == 34) {     // Page down
      selectedItem_ += totalVisibleItems_ - 1;
      showSelectedItem();

    } else if (keyCode == 13) {     // Enter
      acceptedCallback_(items_[selectedItem_].displayValue);

    } else if (keyCode == 27) {     // Escape
      cancelledCallback_();

    } else if (keyCode == 36) {     // Home
      selectedItem_ = 0;
      showSelectedItem();

    } else if (keyCode == 35) {     // End
      selectedItem_ = items_.length - 1;
      showSelectedItem();

    } else {
      return false
    }

    return true;
  }

  this.addItem = addItem;
  this.notifyKeyPress = notifyKeyPress;
  this.show = show;
  this.hide = hide;
}
