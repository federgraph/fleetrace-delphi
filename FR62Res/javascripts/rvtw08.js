var TimingWidget =
{
    init: function() {
        var timingWidget = document.getElementById("ziffern-block");
        var inputs = timingWidget.getElementsByTagName("td");

        for (var i = 0; i < inputs.length; i++) {
            if (inputs[i].id == "record")
                Core.addEventListener(inputs[i], "click", TimingWidget.clickListener);
            else if (inputs[i].id == "clear-bib")
                Core.addEventListener(inputs[i], "click", TimingWidget.clearBib);
            else
                Core.addEventListener(inputs[i], "click", TimingWidget.updateBib);

            TimingWidget.clearBib();
        }
    },
    getBib: function() {
        var bib = document.getElementById("bib");
        return bib.firstChild.nodeValue;
    },
    clearBib: function(event) {
        var bib = document.getElementById("bib");
        bib.firstChild.nodeValue = ""
    },
    updateBib: function(event) {
        var z = this.firstChild.nodeValue;
        var bib = document.getElementById("bib");
        var v = bib.firstChild.nodeValue;
        bib.firstChild.nodeValue = v + z;
    },
    clickTest: function(event) {
        var bib = TimingWidget.getBib();
        if (bib != "" && bib < 10000)
            TimingWidget.writeUpdate("recorded: bib " + bib);
        else
            TimingWidget.writeUpdate("invalid bib");
        TimingWidget.clearBib();
    },
    clickListener: function(event) {
        var bib = TimingWidget.getBib();
        TimingWidget.clearBib();
        if (bib == "" || bib > 10000)
            return;

        var requester;
        try {
            requester = new XMLHttpRequest();
        }
        catch (error) {
            try {
                requester = new ActiveXObject("Microsoft.XMLHTTP");
            }
            catch (error) {
                requester = null;
            }
        }

        if (requester != null) {
            var widgetLink = this;
            widgetLink._timer = setTimeout(function() {
                requester.abort();
                TimingWidget.writeError("The server timed out while making your request.");
            }, 120000);

            var race = document.getElementById("race-param").firstChild.nodeValue;
            var it = document.getElementById("it-param").firstChild.nodeValue;

            var url = "tw08-ajax?";
            url += "race=" + encodeURIComponent(race);
            url += "&it=" + encodeURIComponent(it);
            url += "&bib=" + encodeURIComponent(bib);

            requester.open("GET", url, true);
            requester.onreadystatechange = function() {
                if (requester.readyState == 4) {
                    clearTimeout(widgetLink._timer);

                    if (requester.status == 200 || requester.status == 304) {
                        TimingWidget.writeUpdate(requester.responseText);
                    }
                    else {
                        TimingWidget.writeError("The server was unable to be contacted.");
                    }
                }
            };
            requester.send(null);

            Core.preventDefault(event);
        }
    },
    writeUpdate: function(responseText) {
        var div = document.getElementById("response-text");
        if (div != null)
            div.innerHTML = responseText;
    },
    writeError: function(errorMsg) {
        alert(errorMsg);
    }
};

Core.start(TimingWidget);