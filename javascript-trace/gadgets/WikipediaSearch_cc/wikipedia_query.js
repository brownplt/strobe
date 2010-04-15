// This class will query Google for Wikipedia pages. It uses the Google cached
// pages to scrape from since the Wikipedia official servers are known to be
// very slow. Also, this makes it easier for us to control the server load.
function WikipediaQuery() {
  var QUERY_URL =
    //"http://www.google.com/search?q=cache%3Aen.wikipedia.org%2Fwiki%2F";
    //"http://en.wikipedia.org/wiki/";
    "http://en.wikipedia.org/wiki/Special:Search/";
  var FIRST_PARAGRAPH_REGEX = /<p>(.+)\s*<\/p>/g;
  var IMAGE_URL_REGEX = /img src=\"((?:http\:\/\/upload\.wikimedia\.org)(?:.*?))(\")/g;
  var ARTICLE_NOT_FOUND_REGEX = /^Wikipedia does not have an article with this exact name./;
  var DISAMBIGUATION_PAGE_REGEX = /href=\"\/wiki\/Wikipedia\:Disambiguation/;
  var DISAMBIGUATION_TITLES_REGEX = /<a href=\".*?\" title=\"(.*?)\"./g;
  var ACTION_EDIT_SEARCH = "ACTION=EDIT";
  var MULTIPLE_ARTICLES = "It has been suggested that this article be split into multiple articles accessible";
  var TEMPLATE = "/wiki/Template:";
  var MAIN_PAGE_TITLE = "<title>Main Page - Wikipedia, the free encyclopedia</title>";
  var OPEN_TABLE_TEXT = "<table class=\"messagebox";
  var CLOSE_TABLE_TEXT = "</table";
  var BAD_IMAGES = ["WIKI_LETTER_W.PNG", "LINKFA-STAR.PNG"];

  var MINIMUM_PARAGRAPH_LENGTH = 15;

  var receivedResultCallback_ = null; // (paragraph, imageURL, articleURL)
  var request_ = null;
  var articleURL_ = null;

  var pageText_ = null;
  var originalQuery_ = null;

  function query(name, receivedResultCallback) {
    assert(request_ == null);
    if (request_ != null)
      return;
    assert(receivedResultCallback != null);
    if (receivedResultCallback == null)
      return;

    receivedResultCallback_ = receivedResultCallback;
    originalQuery_ = name;

    // Create a clean and safe query name
    var safeName = originalQuery_.replace(/ /g, "_");
    safeName = encodeURI(safeName);

    request_ = new SimpleHTTPRequest();
    request_.request(QUERY_URL + safeName, onReceivedWebpage);
    articleURL_ = QUERY_URL + safeName;

    return articleURL_;
  }

  function onReceivedWebpage(text) {
    request_ = null;
    pageText_ = text;

    // Verify that the request was successful
    if (pageText_ == null) {
      receivedResultCallback_(null, null, null);
      return;
    }

    // Verify that the page is valid and not a return to the main page
    if (pageText_.indexOf(MAIN_PAGE_TITLE) != -1) {
      receivedResultCallback_(null, null, null);
      return;
    }

    // Check if the text "It has been suggested that this article be split into
    // multiple articles accessible" exists. This means that there is an
    // ambiguation link on the page which will improperly trigger the
    // the disambiguation mode.
    var notDisambiguation = false;
    if (pageText_.indexOf(MULTIPLE_ARTICLES) != -1)
      notDisambiguation = true;

    // Check if this is a disambiguation page. Disambiguation pages have a line
    // at the bottom of the page which mentions that the page is intended for
    // disambiguation.
    if (!notDisambiguation) {
      var disambiguationRegex = DISAMBIGUATION_PAGE_REGEX.exec(pageText_);
      if ((disambiguationRegex != null) && (disambiguationRegex.length == 1)) {
        receivedResultCallback_("", null, null);
        return;
      }
    }

    // Get the first paragraph's text
    var firstParagraph = getArticleFirstParagraph();

    // Check if no suitable first paragraph was found
    if (firstParagraph == "") {
      receivedResultCallback_(null, null, null);
      return;
    }

    // Check if this is a real article of if it's a 'not found' page
    var notFoundRegex = ARTICLE_NOT_FOUND_REGEX.exec(firstParagraph);
    if ((notFoundRegex != null) && (notFoundRegex.length == 1)) {
      receivedResultCallback_(null, null, null);
      return;
    }

    // Get the image URL
    var imageURL = getFirstImageURL();

    receivedResultCallback_(firstParagraph, imageURL, articleURL_);
  }

  // Parses the page text that should already be set in pageText_ and retrieves
  // the text of the first paragraph.
  function getArticleFirstParagraph() {
    // Loop through all paragraph tags in the article until one is found that is
    // suitable for the description
    var result = null;
    var firstParagraph = "";

    while ((result = FIRST_PARAGRAPH_REGEX.exec(pageText_)) != null) {
      if (result.length >= 2) {
        // Check if there is a <td> tag before this <p> tag. This strategic
        // search is very often successful in finding improper <p> tags put in
        // tables which is never the actual first paragraph.
        var openTagLoc = pageText_.substr(0, result.index).lastIndexOf("<");

        if (openTagLoc >= -1) {
          // Check if there is a template link in the potential paragraph. If
          // there is, this is likely not the proper first paragraph.
          if (result[1].indexOf(TEMPLATE) == -1) {
            var tdCheckText = pageText_.substr(openTagLoc,
              result.index - openTagLoc).toUpperCase();

            // Only continue if the TD tag was not found
            if (tdCheckText.indexOf("<TD") == -1) {
              // Take the current text between <p> and </p> tags
              var plainText = htmlToPlainText(result[1]);
              plainText = plainText.replace(/^\s+/g, "");  // Strip leading spaces
              plainText = plainText.replace(/\s+$/g, "");  // Strip trailing spaces

              // Check for minimum length
              if (plainText.length >= MINIMUM_PARAGRAPH_LENGTH) {
                firstParagraph = plainText;
                return firstParagraph;
              }
            }
          }
        }
      }
    }

    return "";
  }

  // Parses the page text to find the first image's URL
  function getFirstImageURL() {
    var result = null;
    var imageURL = "";

    while ((result = IMAGE_URL_REGEX.exec(pageText_)) != null) {
      if (result.length >= 2) {
        // We have a location for a picture but one problem can happen. If the
        // picture is within a messagebox table we're not interested in it since
        // this usually just says "This article is locked to prevent inaccurate
        // data" and just has a picture of a lock (which is not the actual image
        // that we want). Therefore, we check to make sure we're not in a table.
        var lastCloseTableLoc = pageText_.lastIndexOf(CLOSE_TABLE_TEXT, result.index);
        var lastOpenTableLoc = pageText_.lastIndexOf(OPEN_TABLE_TEXT, result.index);

        if (lastCloseTableLoc >= lastOpenTableLoc) {
          // The closest tag is a </table>, which means this image is not
          // already inside a table -- therefore take the URL.
          var URL = result[1];

          // Do not take the URL if it has been blacklisted
          var bad = false;
          for (var i = 0; ((i <= BAD_IMAGES.length - 1) && (!bad)); i++) {
            if (URL.toUpperCase().indexOf(BAD_IMAGES[i]) > -1)
              bad = true;
          }

          if (!bad)
            return result[1];
        }
      }
    }

    return null;
  }

  // Stop a query, if it exists
  function stop() {
    if (request_ != null)
      request_.stop();
  }

  // Get an array of article titles of articles similar to the last query name
  function getDisambiguationArray() {
    var articleTitles = [];

    var result = null;
    var upperQuery = originalQuery_.toUpperCase();

    while ((result = DISAMBIGUATION_TITLES_REGEX.exec(pageText_)) != null) {
      if (result.length >= 2) {
        // If there is a colon in the title, this means it is a special page,
        // therefore do not parse
        if (result[1].indexOf(":") == -1) {
          // Do not accept the item if it has "action=edit" in the url. This
          // means the link is a placeholder and does not actually contain any
          // useful information
          if (result[0].toUpperCase().indexOf(ACTION_EDIT_SEARCH) == -1) {
            // Look for the original query in the title of this article
            if (result[1].toUpperCase().indexOf(upperQuery) != -1) {
              // Since found, add this to the list of valid articles if not
              // already on the list
              var found = false;
              var upperResult = htmlToPlainText(result[1].toUpperCase());

              for (var i = 0; i <= articleTitles.length - 1; i++) {
                if (articleTitles[i].toUpperCase() == upperResult) {
                  found = true;
                  break;
                }
              }

              if (!found)
                articleTitles.push(htmlToPlainText(result[1]));
            }
          }
        }
      }
    }

    return articleTitles.sort();
  }

  // A simple function to get only the text from a webpage. Some small
  // conversions are done to convert some html elements to symbols.
  function htmlToPlainText(item) {
    if (item) {
      // Remove html tags
      item = item.replace(/<([^>]|\n)*>/g, '');

      // Convert symbols
      item = item.replace(/&nbsp;/g, ' ');
      item = item.replace(/&quot;/g, '"');
      item = item.replace(/&amp;/g, '&');
      item = item.replace(/&lt;/g, '<');
      item = item.replace(/&gt;/g, '>');
      item = item.replace(/&#160;/g, ' ');
      return item;
    } else {
      return "";
    }
  }

  this.query = query;
  this.stop = stop;
  this.getDisambiguationArray = getDisambiguationArray;
}

function assert(cond) {
  if (!cond) {
    throw 0;
  }
}
