var req = new XMLHttpRequest();

function deleteResultChild() {
    var result = document.getElementById("result");
    child_list = result.childNodes;
    for (var i = 0; i < child_list.length; i += 1)
    {
        result.removeChild(child_list[i]);
    }
}

function search() {
    var result = document.getElementById("result");
    var jsondata = JSON.parse(req.responseText);
    if (jsondata.messages == null)
    {
        result.appendChild(document.createTextNode("No result"));
    }
    else
    {
        for (var i = 0; i < jsondata.messages.length; i++) {
            var date = jsondata.messages[i][0];
            var channel = jsondata.messages[i][1];
            var nick = jsondata.messages[i][3].split("!")[0];
            var message = jsondata.messages[i][4];
            result.appendChild(document.createTextNode(date + " " +
                                                      channel + " " +
                                                      nick + " " +
                                                      message));
            result.appendChild(document.createElement("br"));
        }
    }
}

function popup() {
    req.open(
      "GET",
      "http://bb.isasaka.net/api/search?q=" +
	      encodeURIComponent(document.getElementById("q").value),
        false);
    req.onload = search;
    req.send(null);
}

function use_options() {
  var query = localStorage["query"];
  if (!query) {
    return;
  }
  var q = document.getElementById("q");
  q.value = query;
}