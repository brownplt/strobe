var HISTORY_OPTIONS_NAME = "history";
var MAX_HISTORY_ENTRIES = 5;
var HISTORY_DETAILS_HTML = DEFAULT_HTML_STYLE +
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

  // Draw the menu
  "<div id=\"vertmenu\">" +
  RECENT_SEARCHES +
  "<ul>" +
  "[MENUITEMS]";
  "</ul>" +
  "</div>" +
  "</body>";
var HISTORY_DETAILS_HTML_ITEM =
  "<li><a href=\"\" onclick=\"window.external.selectedItem('[TEXT]'); return false;\">&raquo; [TEXT]</a></li>";
var MAX_HISTORY_ITEMS = 5;

// This class takes care of tracking the wikipedia history. This includes
// storing/loading keywords and showing the history in sidebar or not
function WikipediaHistory(historyDiv_, selectedHistoryItemCallback_) {
  var historyListbox_ = null;
  var items_ = [];
  var inSidebar_ = (historyDiv_ == null);
  var ieDetailsView_ = null;

  // Load the history from the options
  if (options.exists(HISTORY_OPTIONS_NAME)) {
    items_ = options(HISTORY_OPTIONS_NAME).split('|');
    if ((items_.length == 1) && (items_[0] == ""))
      items_.splice(0, 1);
  }

  // Show the history box with all history items
  function show() {
    // If there are no items, do not show
    if (items_.length == 0)
      return;

    if (!inSidebar_) {
      // Verify that the history is not already up
      if (historyListbox_ != null)
        return;

      // Add the items to the listbox in reverse order
      historyListbox_ = new ListBox(historyDiv_, onAcceptedEntry, onCancelledEntry);
      for (var i = items_.length - 1; i >= 0; i--)
        historyListbox_.addItem(items_[i]);

      historyListbox_.show(null);

    } else {
      // Generate the list of items
      var itemsHtml = "";
      for (var i = items_.length - 1; i >= 0; i--) {
        itemsHtml += HISTORY_DETAILS_HTML_ITEM.replace(/\[TEXT\]/g, items_[i]);
      }

      // Generate the total html
      var html = HISTORY_DETAILS_HTML.replace("[MENUITEMS]", itemsHtml);

      // Show the details view
      ieDetailsView_ = new DetailsView();
      ieDetailsView_.html_content = true;
      ieDetailsView_.setContent("", undefined, html, false, 0);

      var externalObject = new Object();
      externalObject.selectedItem = onSelectedItem;
      ieDetailsView_.external = externalObject;

      pluginHelper.showDetailsView(ieDetailsView_, WIKIPEDIA_SEARCH, gddDetailsViewFlagNone,
        onHistoryDetailsViewFeedback);
    }
  }

  function onSelectedItem(itemText) {
    // Callback to the parent. Note that if the parent does a hide() on this
    // object nothing will happen since the details view windows cannot be
    // closed.
    selectedHistoryItemCallback_(itemText);
  }

  // Called when the history details view of the IE gets clicked on
  function onHistoryDetailsViewFeedback(detailsViewFlags) {
  }

  // Hide the history box if it's open
  function hide() {
    if (!inSidebar_) {
      if (historyListbox_ != null) {
        historyListbox_.hide();
        historyListbox_ = null;
      }
    }
  }

  // Called when the user accepts a selection in the listbox
  function onAcceptedEntry(text) {
    hide();
    selectedHistoryItemCallback_(text);
  }

  // Called when the user cancels a selection in the listbox
  function onCancelledEntry() {
    hide();
  }

  // Add an item to the history options object
  function addHistoryItem(text) {
    text = text.replace(/\|/g, " ");

    // If the item is already the most recent, do not add it again
    if (items_.length >= 1) {
      if (items_[items_.length - 1] == text)
        return;
    }

    // If there are already the maximum amount of items, remove the oldest
    if (items_.length == MAX_HISTORY_ENTRIES)
      items_.splice(0, 1);

    items_.push(text);
    saveHistoryItems();
  }

  // Save the current items list to the history options object
  function saveHistoryItems() {
    //options(HISTORY_OPTIONS_NAME) = items_.join("|");
    options.putValue(HISTORY_OPTIONS_NAME, items_.join("|"));
  }

  this.show = show;
  this.hide = hide;
  this.addHistoryItem = addHistoryItem;
}
