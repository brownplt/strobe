var DISAMBIGUATION_DETAILS_HTML = DEFAULT_HTML_STYLE +
  // Set the style for the list
  "<style type=\"text/css\">" +
  "#vertmenu {" +
  "  font-family: Verdana, Arial, Helvetica, sans-serif;" +
  "  font-size: 11px;" +
  "  color: #666666;" +
  "  width: 100%;" +
  "  padding: 0px;" +
  "  margin: 0px;" +
  "}" +
  "#vertmenu ul {" +
  "  list-style: none;" +
  "  margin: 0px;" +
  "  padding: 0px;" +
  "  border: none;" +
  "}" +
  "#vertmenu ul li {" +
  "  margin: 0px;" +
  "  padding: 0px;" +
  "}" +
  "#vertmenu ul li a {" +
  "  font-size: 80%;" +
  "  display: block;" +
  "  padding: 2px 0px 2px 7px;" +
  "  text-decoration: none;" +
  "  color: #008800;" +
  "  width:100%;" +
  "}" +
  "#vertmenu ul li a:hover, #vertmenu ul li a:focus {" +
  "  color: #000000;" +
  "  background-color: #eeeeee;" +
  "}" +
  "</style>" +
  "<body>" +

  // Center the menu vertically and horizontally
  //"<div class=\"greenBorder\" style=\"display: table; height: 100%; _position: relative; overflow: hidden;\">" +
  //"<div style=\" _position: absolute; _top: 50%; _left: 50%; display: table-cell; vertical-align: middle;\">" +
  //"<div class=\"greenBorder\" style=\" _position: relative; _top: -50%; _left: -50%\">" +

  // Draw the menu
  "<div align=\"center\">" +
  "<div id=\"vertmenu\" align=\"left\">" +
  DISAMBIGUATION +
  "<ul>" +
  "[MENUITEMS]";
  "</ul>" +
  "</div>" +
  "</div>" +
  "</body>";
var DISAMBIGUATION_DETAILS_HTML_ITEM =
  "<li><a href=\"\" onclick=\"window.external.selectedItem('[TEXT]'); return false;\">&raquo; [TEXT]</a></li>";

// This class takes care of managing disambiguation using a listbox
function WikipediaDisambiguation(disambiguationDiv_, textboxElement_,
    textboxOnKeyDown_, textboxOnKeyPress_, selectedDisambiguationItemCallback_) {
  var listbox_ = null;
  var ieDetailsView_ = null;

  // It is assumed that if there is no onKeyPress event set, this won't be
  // reacting to search-as-you-type, therefore must be in the sidebar
  var inSidebar_ = (textboxOnKeyPress_ == null);

  // Verify that valid callbacks are given
  if (textboxOnKeyDown_ == null) { textboxOnKeyDown_ = function () {} };
  if (textboxOnKeyPress_ == null) { textboxOnKeyPress_ = function () {} };

  // Show the disambiguation dropdown
  function show(arrayOfItems) {
    // We are creating a timer here to avoid a bug that does a slow redraw when
    // show() is called from an onData callback from XMLHttpRequest.
    setTimeout(function () {
      if (!inSidebar_) {
        // Create the listbox and show all the items
        listbox_ = new ListBox(historydiv, onAcceptedDisambiguation,
          onCancelledDisambiguation);
        for (var i = 0; i <= arrayOfItems.length - 1; i++)
          listbox_.addItem(arrayOfItems[i]);
        listbox_.show(0);

        // Bind to the textbox control to be able to use the up and down arrow keys
        textboxElement_.onKeyDown = onTextBoxKeyDown;
        textboxElement_.onKeyPress = function () {};
      } else {
         // Generate the list of items
        var itemsHtml = "";
        for (var i = 0; i <= arrayOfItems.length - 1; i++) {
          itemsHtml += DISAMBIGUATION_DETAILS_HTML_ITEM.replace(/\[TEXT\]/g, arrayOfItems[i]);
        }

        // Generate the total html
        var html = DISAMBIGUATION_DETAILS_HTML.replace("[MENUITEMS]", itemsHtml);

        // Show the details view
        ieDetailsView_ = new DetailsView();
        ieDetailsView_.html_content = true;
        ieDetailsView_.setContent("", undefined, html, false, 0);

        var externalObject = new Object();
        externalObject.selectedItem = onSelectedItem;
        ieDetailsView_.external = externalObject;

        pluginHelper.showDetailsView(ieDetailsView_, WIKIPEDIA_SEARCH, gddDetailsViewFlagNone,
          onDisambiguationDetailsViewFeedback);
      }
    }, 1);
  }

  // Hide the disambiguation dropdown
  function hide() {
    // Only destroy everything once the current event is completed (this usually
    // is because hide() is called on a keyboard event and not all keyboard
    // events are completed, so if we were to reset callbacks, the remaining
    // callbacks would be wrongly called!)
    setTimeout(function () {
      // Remove the listbox
      if (listbox_ != null) {
        listbox_.hide();
        listbox_ = null;
      }

      // Restore proper onKeyPress function
      textboxElement_.onKeyDown = textboxOnKeyDown_;
      textboxElement_.onKeyPress = textboxOnKeyPress_;
    }, 1);
  }

  function onSelectedItem(itemText) {
    // Callback to the parent. Note that if the parent does a hide() on this
    // object nothing will happen since the details view windows cannot be
    // closed.
    selectedDisambiguationItemCallback_(itemText);
  }

  // Called when the disambiguation details view of the IE gets clicked on
  function onDisambiguationDetailsViewFeedback(detailsViewFlags) {
    if (detailsViewFlags == 0)
      selectedDisambiguationItemCallback_(null);
  }


  // Called when the user types anything into the textbox
  function onTextBoxKeyDown() {
    if (listbox_.notifyKeyPress(event.keyCode))
      event.returnValue = false;
  }

  // Called when the user accepts a certain disambiguation entry
  function onAcceptedDisambiguation(value) {
    hide();
    selectedDisambiguationItemCallback_(value);
  }

  // Called when the user cancels disambiguation entry
  function onCancelledDisambiguation() {
    hide();
    selectedDisambiguationItemCallback_(null);
  }

  this.show = show;
  this.hide = hide;
}
