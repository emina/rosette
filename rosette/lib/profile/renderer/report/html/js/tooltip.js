var tooltip;
(function (tooltip) {
    var tooltipDiv;
    var PADDING = 4;
    var willShow = false;
    function init() {
        tooltipDiv = document.createElement("div");
        tooltipDiv.className = "tooltip";
        tooltipDiv.style.position = "absolute";
        tooltipDiv.style.top = "0px";
        tooltipDiv.style.left = "0px";
        tooltipDiv.style.display = "none";
        document.body.insertAdjacentElement("beforeend", tooltipDiv);
    }
    tooltip.init = init;
    function show(title, elt, side) {
        if (side != "top") {
            console.error("only 'top' side tooltip supported");
            return;
        }
        if (title == "") {
            title = elt.dataset["title"];
        }
        tooltipDiv.innerText = title;
        tooltipDiv.style.display = "block";
        var tRect = tooltipDiv.getBoundingClientRect();
        var eRect = getBoundingViewportRect(elt);
        var vRect = getViewportRect();
        var x = eRect.left + eRect.width / 2 - tRect.width / 2;
        if (x < vRect.left) {
            x = vRect.left + PADDING;
        }
        else if (x + tRect.width > vRect.right) {
            x = vRect.right - tRect.width - PADDING;
        }
        var y = eRect.top - tRect.height - PADDING;
        if (y < vRect.top) {
            y = vRect.top + PADDING;
        }
        else if (y + tRect.height > vRect.bottom) {
            y = vRect.bottom - tRect.height - PADDING;
        }
        tooltipDiv.style.transform = "translate(" + x + "px," + y + "px)";
        willShow = false;
    }
    tooltip.show = show;
    function showWithDelay(title, elt, side, delay) {
        willShow = true; // make sure user didn't leave the element during the delay
        setTimeout(function () { if (willShow)
            show(title, elt, side); }, delay);
    }
    tooltip.showWithDelay = showWithDelay;
    function getBoundingViewportRect(elt) {
        var rect = elt.getBoundingClientRect();
        return {
            top: rect.top + window.pageYOffset,
            bottom: rect.bottom + window.pageYOffset,
            left: rect.left + window.pageXOffset,
            right: rect.right + window.pageXOffset,
            width: rect.width,
            height: rect.height
        };
    }
    function getViewportRect() {
        var rect = document.documentElement.getBoundingClientRect();
        return {
            top: window.pageYOffset,
            left: window.pageXOffset,
            bottom: window.pageYOffset + rect.height,
            right: window.pageXOffset + rect.width,
            width: rect.width,
            height: rect.height
        };
    }
    function hide() {
        tooltipDiv.style.display = "none";
        willShow = false;
    }
    tooltip.hide = hide;
})(tooltip || (tooltip = {}));
//# sourceMappingURL=tooltip.js.map