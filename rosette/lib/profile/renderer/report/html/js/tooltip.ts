namespace tooltip {
    var tooltipDiv: HTMLDivElement;
    const PADDING = 4;
    var willShow = false;

    export function init() {
        tooltipDiv = document.createElement("div");
        tooltipDiv.className = "tooltip";
        tooltipDiv.style.position = "absolute";
        tooltipDiv.style.top = "0px";
        tooltipDiv.style.left = "0px";
        tooltipDiv.style.display = "none";
        document.body.insertAdjacentElement("beforeend", tooltipDiv);
    }

    export function show(title: string, elt: HTMLElement, side: string) {
        if (side != "top") {
            console.error("only 'top' side tooltip supported");
            return;
        }
        if (title == "") {
            title = elt.dataset["title"];
        }

        tooltipDiv.innerText = title;
        tooltipDiv.style.display = "block";
        let tRect = tooltipDiv.getBoundingClientRect();
        let eRect = getBoundingViewportRect(elt);
        let vRect = getViewportRect();

        let x = eRect.left + eRect.width/2 - tRect.width/2;
        if (x < vRect.left) {
            x = vRect.left + PADDING;
        } else if (x + tRect.width > vRect.right) {
            x = vRect.right - tRect.width - PADDING;
        }

        let y = eRect.top - tRect.height - PADDING;
        if (y < vRect.top) {
            y = vRect.top + PADDING;
        } else if (y + tRect.height > vRect.bottom) {
            y = vRect.bottom - tRect.height - PADDING;
        }

        tooltipDiv.style.transform = `translate(${x}px,${y}px)`;

        willShow = false;
    }

    export function showWithDelay(title: string, elt: HTMLElement, side: string, delay: number) {
        willShow = true;  // make sure user didn't leave the element during the delay
        setTimeout(() => { if (willShow) show(title, elt, side) }, delay);
    }

    function getBoundingViewportRect(elt: HTMLElement) {
        let rect = elt.getBoundingClientRect();
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
        let rect = document.documentElement.getBoundingClientRect();
        return {
            top: window.pageYOffset,
            left: window.pageXOffset,
            bottom: window.pageYOffset + rect.height,
            right: window.pageXOffset + rect.width,
            width: rect.width,
            height: rect.height
        };
    }

    export function hide() {
        tooltipDiv.style.display = "none";
        willShow = false;
    }
}