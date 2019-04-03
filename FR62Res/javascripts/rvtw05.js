var TimingWidget =
{
    init: function() {
        var timingWidget = document.getElementById("ziffern-block");
        var inputs = timingWidget.getElementsByTagName("input");

        for (var i = 0; i < inputs.length; i++) {
            if (inputs[i].value == "M")
                Core.addEventListener(inputs[i], "click", TimingWidget.clickM);
            else if (inputs[i].value == "P")
                Core.addEventListener(inputs[i], "click", TimingWidget.clickP);
            else
                Core.addEventListener(inputs[i], "click", TimingWidget.clickListener);
        }
        TimingWidget.updateBtnValue(-10);
    },
    updateBtnValue: function(offset) {
        var timingWidget = document.getElementById("ziffern-block");
        var inputs = timingWidget.getElementsByTagName("input");

        for (var i = 0; i < inputs.length; i++) {
            if (inputs[i].value == "M")
                ;
            else if (inputs[i].value == "P")
                ;
            else {
                var v = inputs[i].value;
                v = Math.floor(v);
                v = v + offset;
                if (v > -1)
                    inputs[i].value = v;
            }
        }
    },
    clickM: function(event) {
        TimingWidget.updateBtnValue(-10);
    },
    clickP: function(event) {
        TimingWidget.updateBtnValue(10);
    },
    clickTester: function(event) {
        var bib = this.value;
        TimingWidget.writeUpdate("clicked: Bib " + bib);
    },
    clickListener: function(event) {
        try {
            var requester = new XMLHttpRequest();
        }
        catch (error) {
            try {
                var requester = new ActiveXObject("Microsoft.XMLHTTP");
            }
            catch (error) {
                var requester = null;
            }
        }

        if (requester != null) {
            var widgetLink = this;
            widgetLink._timer = setTimeout(function() {
                requester.abort();
                TimingWidget.writeError("The server timed out while making your request.");
            }, 120000);

            var bib = this.value;
            var race = document.getElementById("race-param").firstChild.nodeValue;
            var it = document.getElementById("it-param").firstChild.nodeValue;

            var url = "tw05-ajax?";
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