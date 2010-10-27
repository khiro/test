function content() {
    var selText = "";
    if (window.getSelection) {      // Firefox, Opera, Google Chrome and Safari
        if (document.activeElement && 
            (document.activeElement.tagName.toLowerCase () == "textarea" || 
             document.activeElement.tagName.toLowerCase () == "input")) 
        {
            var text = document.activeElement.value;
            selText = text.substring (document.activeElement.selectionStart, 
                                      document.activeElement.selectionEnd);
        }
        else {
            var selRange = window.getSelection ();
            selText = selRange.toString ();
        }
    }
    if (selText.length > 0 || selText == " ") {
        alert(selText.length + ":" + selText);
        chrome.extension.sendRequest({action: 'translate',
                                      data: selText}, function(response) {
          alert(response.result);
        });
    }
}

function addEvent() {
    if (document.addEventListener)
    {
        document.addEventListener("dblclick", content, false);
    }
    else if (document.attachEvent)
    {
        document.attachEvent("dblclick", content);
    }
}

addEvent();

