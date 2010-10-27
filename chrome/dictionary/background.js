function xhr(data, callback) {
  var req = new XMLHttpRequest();
  req.onerror = function(error) {
    console.dir(error);
  }
  req.open(
    "GET",
    "http://eow.alc.co.jp/" +
	data +
	"/UTF-8/",
    false);
  req.onload = function() {
     var div = document.createElement("div");
     div.innerHTML = req.responseText;
     var midashi_list = div.getElementsByClassName("mr_10");
     callback({result: midashi_list[0].innerText});
  };

  req.send(null);
}

chrome.extension.onRequest.addListener(
  function(request, sender, sendResponse) {
//    console.log(sender.tab ?
//                "from a content script:" + sender.tab.url :
//                "from the extension");
    if (request.action == "translate")
      //sendResponse({result: request.data});
      xhr(request.data, sendResponse);
    else
      sendResponse({}); // snub them.
  });