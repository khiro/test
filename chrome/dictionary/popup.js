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
  result.appendChild(document.createTextNode("(before)"));
  //var resultlist = req.responseXML.getElementsByTagName("resultList");
  //result.innerHTML = req.responseText;

  var div = document.createElement("div");
  div.innerHTML = req.responseText;
  var midashi_list = div.getElementsByClassName("mr_10");
  result.innerHTML = midashi_list[0].innerHTML;
  //  result.appendChild(midashi_list[0]);
  result.appendChild(document.createTextNode("(after)"));
}

function popup() {
  req.onerror = function(error) {
    console.dir(error);
  }
  req.open(
    "GET",
    "http://eow.alc.co.jp/" +
	document.getElementById("q").value +
	"/UTF-8/",
    false);
  req.onload = search;
  req.send(null);
}

