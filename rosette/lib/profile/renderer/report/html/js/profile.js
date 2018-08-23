var profile;
(function (profile) {
    var calltable;
    (function (calltable) {
        var scoreColumns = [
            { type: "excl", column: "time", name: "Time (ms)", description: "Total time spent in this function (but not in descendent calls)", score: true },
            { type: "excl", column: "term-count", name: "Term Count", description: "Number of symbolic terms created", score: true },
            { type: "excl", column: "unused-terms", name: "Unused Terms", description: "Number of symbolic terms created that were never used for solving", score: true },
            { type: "excl", column: "union-size", name: "Union Size", description: "Total number of branches in all symbolic unions created", score: true },
            { type: "excl", column: "merge-cases", name: "Merge Cases", description: "Number of branches used during merging", score: true },
        ];
        var DOM_ROW_KEY = "symproRowObject";
        var PRUNE_SCORE_FACTOR = 0.01; // < 1% of max score = pruned
        var colorScheme = makeScoreColorScheme(["#000000", "#FD893C", "#D9002C"]);
        var tableSorter;
        var contextDepth = 0;
        var useSignatures = false;
        var useCallsites = false;
        var pruneSmallRows = true;
        var idToRow = {};
        function initCallTable() {
            renderTableHeaders();
            analysis.setAggregate(true);
            analysis.setColumns(scoreColumns);
            analysis.registerAnalysisCallback(receiveData);
        }
        calltable.initCallTable = initCallTable;
        function renderTableHeaders() {
            var thead = document.querySelector("#calltable thead");
            var tr = document.createElement("tr");
            makeCell("Function", tr, "th");
            var scoreCell = makeCell("Score", tr, "th");
            scoreCell.className = "sort-default score";
            scoreCell.id = "calltable-score-header";
            var keys = [];
            for (var i = 0; i < scoreColumns.length; i++) {
                var c = scoreColumns[i];
                var cell = makeCell(c.name, tr, "th");
                if (c.description) {
                    cell.dataset["title"] = c.description;
                    cell.addEventListener("mouseover", function (evt) { return tooltip.showWithDelay("", evt.target, "top", 100); });
                    cell.addEventListener("mouseout", function (evt) { return tooltip.hide(); });
                }
                var checkbox = document.createElement("input");
                checkbox.type = "checkbox";
                checkbox.checked = true;
                checkbox.className = "score-checkbox";
                checkbox.value = i.toString();
                checkbox.addEventListener("change", scoreSelectCallback);
                checkbox.addEventListener("click", function (evt) { return evt.stopPropagation(); }); // prevent tablesorter from firing
                cell.insertAdjacentElement("beforeend", checkbox);
            }
            thead.insertAdjacentElement("beforeend", tr);
            // set up configuration controls
            var config = document.getElementById("calltable-config");
            // aggregate checkbox
            var agg = config.querySelector("#calltable-aggregate");
            agg.checked = true;
            agg.addEventListener("change", configChangeCallback);
            // context slider
            var ctx = config.querySelector("#calltable-context");
            ctx.value = "0";
            var isIE = !!navigator.userAgent.match(/Trident/g) || !!navigator.userAgent.match(/MSIE/g);
            ctx.addEventListener(isIE ? "change" : "input", configChangeCallback);
            // context count
            var ctxn = config.querySelector("#calltable-context-n");
            ctxn.textContent = "0";
            // filter checkbox
            var fil = config.querySelector("#calltable-prune");
            fil.checked = true;
            fil.addEventListener("change", configChangeCallback);
            // collapse rosette checkbox
            var clr = config.querySelector("#calltable-collapse-rosette");
            clr.checked = false;
            clr.addEventListener("change", configChangeCallback);
            // collapse solver checkbox
            var cls = config.querySelector("#calltable-collapse-solver");
            cls.checked = false;
            cls.addEventListener("change", configChangeCallback);
            // signature checkbox
            var sig = config.querySelector("#calltable-signature");
            sig.checked = false;
            sig.addEventListener("change", configChangeCallback);
            // callsites checkbox
            var css = config.querySelector("#calltable-callsites");
            css.checked = false;
            css.parentElement.style.display = "none";
            css.addEventListener("change", configChangeCallback);
            // score checkbox
            var sco = config.querySelector("#calltable-show-scoreboxes");
            sco.checked = false;
            sco.addEventListener("change", configChangeCallback);
            // more config
            var more = config.querySelector("#calltable-config-more");
            more.style.display = "none";
            var moreLink = config.querySelector("#calltable-config-toggle-more");
            moreLink.addEventListener("click", toggleMoreCallback);
            // attach event handler for table body
            var tbody = document.querySelector("#calltable tbody");
            tbody.addEventListener("mouseover", hoverCallback);
            tbody.addEventListener("mouseout", hoverCallback);
            tableSorter = new Tablesort(document.getElementById("calltable"), { descending: true });
        }
        function configChangeCallback(evt) {
            var elt = this;
            if (elt.id == "calltable-aggregate") {
                var lbl = document.getElementById("calltable-callsites").parentElement;
                lbl.style.display = (elt.checked ? "none" : "");
                analysis.setAggregate(elt.checked);
            }
            else if (elt.id == "calltable-context") {
                contextDepth = elt.value == elt.max ? -1 : parseInt(elt.value);
                var ctxn = document.getElementById("calltable-context-n");
                ctxn.textContent = contextDepth >= 0 ? elt.value : "∞";
                analysis.setContextDepth(contextDepth);
            }
            else if (elt.id == "calltable-prune") {
                pruneSmallRows = elt.checked;
                analysis.refresh();
            }
            else if (elt.id == "calltable-collapse-rosette") {
                analysis.setCollapseRosette(elt.checked);
            }
            else if (elt.id == "calltable-collapse-solver") {
                stackgraph.setCollapseSolverTime(elt.checked);
                analysis.setCollapseSolver(elt.checked);
            }
            else if (elt.id == "calltable-signature") {
                useSignatures = elt.checked;
                analysis.setSignatures(elt.checked);
            }
            else if (elt.id == "calltable-callsites") {
                useCallsites = elt.checked;
                analysis.refresh();
            }
            else if (elt.id == "calltable-show-scoreboxes") {
                var boxes = document.querySelectorAll("#calltable .score-checkbox");
                for (var i = 0; i < boxes.length; i++) {
                    boxes[i].style.display = elt.checked ? "initial" : "none";
                    if (!elt.checked) {
                        scoreColumns[i].score = true;
                    }
                }
                if (!elt.checked) {
                    analysis.setColumns(scoreColumns);
                }
            }
            windowResizeCallback();
        }
        function toggleMoreCallback(evt) {
            evt.preventDefault();
            var more = document.getElementById("calltable-config-more");
            var elt = this;
            if (more.style.display == "none") {
                more.style.display = "block";
                this.textContent = "[Less]";
            }
            else {
                more.style.display = "none";
                this.textContent = "[More]";
            }
        }
        function hoverCallback(evt) {
            if (evt.type == "mouseover") {
                var tgt = evt.target;
                while (tgt && tgt.tagName != "TR")
                    tgt = tgt.parentElement;
                stackgraph.calltableHoverCallback(tgt[DOM_ROW_KEY].allNodes);
            }
            else {
                stackgraph.calltableHoverCallback([]);
            }
        }
        function scoreSelectCallback(evt) {
            var elt = this;
            var idx = parseInt(elt.value);
            scoreColumns[idx].score = elt.checked;
            analysis.setColumns(scoreColumns);
        }
        function stackgraphHoverCallback(node, enter) {
            var rows = document.querySelectorAll("#calltable tbody tr");
            for (var i = 0; i < rows.length; i++) {
                var row = rows[i];
                row.className = "";
            }
            if (enter) {
                var hiRow = idToRow[node.id];
                if (hiRow) {
                    hiRow.className = "calltable-highlight";
                }
            }
        }
        calltable.stackgraphHoverCallback = stackgraphHoverCallback;
        function receiveData(state) {
            renderTableRows(state.rows, state.aggregate, state.maxScore);
        }
        function renderPrettySource(source, cell, iscallsite) {
            if (iscallsite === void 0) { iscallsite = false; }
            if (source) {
                var prettySource = getPrettySource(source);
                if (iscallsite)
                    prettySource = "from " + prettySource;
                var sourceSpan = makeCell(prettySource, cell, "span");
                sourceSpan.className = "source";
                sourceSpan.title = source;
            }
        }
        function getPrettySource(source) {
            var sourceParts = source.split(":");
            if (sourceParts.length != 3)
                return source;
            var file = sourceParts[0], line = sourceParts[1], col = sourceParts[2];
            var fileParts = file.split("/");
            return fileParts[fileParts.length - 1] + ":" + line;
        }
        function renderTableRows(rows, aggregate, maxScore) {
            // remove old rows
            var tbody = document.querySelector("#calltable tbody");
            while (tbody.firstChild)
                tbody.removeChild(tbody.firstChild);
            idToRow = {};
            // create new rows
            for (var _i = 0, rows_1 = rows; _i < rows_1.length; _i++) {
                var r = rows_1[_i];
                var row = document.createElement("tr");
                if (pruneSmallRows && r.score < maxScore * PRUNE_SCORE_FACTOR) {
                    continue;
                }
                // render the function name
                var nameCell = makeCell(r.function, row);
                nameCell.className = "name";
                if (useCallsites && !aggregate) {
                    renderPrettySource(r.node.callsite, nameCell, true);
                }
                else {
                    renderPrettySource(r.node.source, nameCell);
                }
                if (aggregate) {
                    var txt = r.allNodes.length > 1 ? formatNum(r.allNodes.length) + " calls" : "1 call";
                    var numCallsSpan = makeCell(txt, nameCell, "span");
                    numCallsSpan.className = "numcalls";
                }
                // maybe render the signature
                if (useSignatures) {
                    var inputs = r.node.inputs["signature"] || [];
                    var outputs = r.node.outputs["signature"] || [];
                    var istr = inputs.map(function (s) { return s[0].toUpperCase(); }).join("");
                    var ostr = outputs.map(function (s) { return s[0].toUpperCase(); }).join("");
                    var span = makeCell(istr + "→" + ostr, nameCell, "span");
                    span.className = "signature";
                }
                // render the call context if requested
                if (contextDepth != 0) {
                    var contextList = document.createElement("ul");
                    contextList.className = "context-list";
                    var n = contextDepth, curr = r.node;
                    if (n == -1) { // if "infinite", collapse recursion
                        var count = 0;
                        while (curr = curr.parent) {
                            if (curr.parent && curr.parent.name == curr.name && curr.parent.source == curr.source) {
                                count += 1;
                            }
                            else {
                                var name_1 = curr.name + (count > 0 ? " ×" + (count + 1) : "");
                                var li = makeCell(name_1, contextList, "li");
                                count = 0;
                                renderPrettySource(curr.source, li);
                            }
                        }
                    }
                    else {
                        while (n-- != 0 && (curr = curr.parent)) {
                            var li = makeCell(curr.name, contextList, "li");
                            renderPrettySource(curr.source, li);
                        }
                    }
                    nameCell.insertAdjacentElement("beforeend", contextList);
                }
                // score cell
                var scoreBar = document.createElement("div");
                scoreBar.className = "scorebar";
                scoreBar.style.width = (r.score / scoreColumns.length * 100) + "%";
                scoreBar.style.backgroundColor = colorScheme(r.score);
                var scoreBarCell = document.createElement("div");
                scoreBarCell.className = "scorecell-bar";
                scoreBarCell.insertAdjacentElement("beforeend", scoreBar);
                var scoreSpan = document.createElement("span");
                scoreSpan.textContent = formatNum(r.score, 1, false, true);
                scoreSpan.style.color = colorScheme(r.score);
                var scoreSpanCell = document.createElement("div");
                scoreSpanCell.className = "scorecell-score";
                scoreSpanCell.insertAdjacentElement("beforeend", scoreSpan);
                var scoreCell = makeCell("", row);
                scoreCell.dataset["sort"] = r.score.toFixed(16);
                scoreCell.insertAdjacentElement("beforeend", scoreSpanCell);
                scoreCell.insertAdjacentElement("beforeend", scoreBarCell);
                // data columns
                for (var _a = 0, _b = r.columns; _a < _b.length; _a++) {
                    var k = _b[_a];
                    makeCell(formatNum(k, 0), row);
                }
                // attach row object to the row
                row[DOM_ROW_KEY] = r;
                // record IDs
                for (var _c = 0, _d = r.allNodes; _c < _d.length; _c++) {
                    var n = _d[_c];
                    idToRow[n.id] = row;
                }
                tbody.insertAdjacentElement("beforeend", row);
            }
            // refresh the sort
            tableSorter.refresh();
        }
        function makeCell(str, row, type) {
            if (type === void 0) { type = "td"; }
            var elt = document.createElement(type);
            elt.textContent = str;
            row.insertAdjacentElement('beforeend', elt);
            return elt;
        }
    })(calltable || (calltable = {}));
    var stackgraph;
    (function (stackgraph_1) {
        var stackgraph;
        var colorScheme = makeScoreColorScheme(["#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026"]);
        var solverHighlightColor = "#E3F2FF";
        var lastWidth = 0;
        var useDiscontinuities = false;
        function initStackGraph() {
            // build stackgraph
            var STACK_WIDTH = document.getElementById("stackgraph").clientWidth;
            var STACK_HEIGHT = 270;
            lastWidth = STACK_WIDTH;
            stackgraph = d3_stackgraph.stackGraph("#stackgraph");
            stackgraph.width(STACK_WIDTH).height(STACK_HEIGHT);
            stackgraph.clickHandler(clickCallback);
            stackgraph.hoverHandler(hoverCallback);
            stackgraph.color(nodeColorCallback);
            stackgraph.textColor(nodeTextColorCallback);
            stackgraph.render();
            analysis.registerAnalysisCallback(receiveData);
            analysis.registerZoomCallback(receiveZoom);
        }
        stackgraph_1.initStackGraph = initStackGraph;
        function makeHighlightList(state) {
            var ret = [];
            for (var _i = 0, _a = state.solverCalls; _i < _a.length; _i++) {
                var call = _a[_i];
                var finish = typeof call.finish === "undefined" ? state.root.finish : call.finish;
                var dt = formatNum(finish - call.start, 1);
                var summary = "";
                if (call.type == data.SolverCallType.SOLVE) {
                    var sat = typeof call.sat === "undefined" ? " (pending)" : (call.sat ? " (SAT)" : " (UNSAT)");
                    summary = "Solver call" + sat + ": " + dt + "ms";
                }
                else if (call.type == data.SolverCallType.ENCODE) {
                    summary = "Solver encoding: " + dt + "ms";
                }
                else if (call.type == data.SolverCallType.FINITIZE) {
                    summary = "Solver finitization: " + dt + "ms";
                }
                ret.push({
                    start: call.start,
                    finish: finish,
                    color: solverHighlightColor,
                    summary: summary
                });
            }
            return ret;
        }
        function makeDiscontinuityList(state) {
            var ret = [];
            for (var _i = 0, _a = state.solverCalls; _i < _a.length; _i++) {
                var call = _a[_i];
                var finish = typeof call.finish === "undefined" ? state.root.finish : call.finish;
                ret.push([call.start, finish]);
            }
            return ret;
        }
        function receiveData(state) {
            if (state.root)
                stackgraph.data(state.root);
            stackgraph.highlights(makeHighlightList(state));
            var discs = useDiscontinuities ? makeDiscontinuityList(state) : [];
            stackgraph.discontinuities(discs);
            stackgraph.render();
        }
        function receiveZoom(node) {
            stackgraph.zoom(node);
        }
        function clickCallback(node) {
            analysis.zoomTo(node);
        }
        function windowResizeCallback() {
            var width = document.getElementById("stackgraph").clientWidth;
            if (width != lastWidth) {
                stackgraph.width(width).render();
                lastWidth = width;
            }
        }
        stackgraph_1.windowResizeCallback = windowResizeCallback;
        function calltableHoverCallback(nodes) {
            stackgraph.highlightData(nodes);
        }
        stackgraph_1.calltableHoverCallback = calltableHoverCallback;
        function setCollapseSolverTime(use) {
            useDiscontinuities = use;
        }
        stackgraph_1.setCollapseSolverTime = setCollapseSolverTime;
        function hoverCallback(node, enter) {
            calltable.stackgraphHoverCallback(node, enter);
        }
        function nodeColorCallback(node) {
            return colorScheme(node.score);
        }
        function nodeTextColorCallback(node) {
            var col = d3.color(colorScheme(node.score)).rgb();
            var yiq = ((col.r * 299) + (col.g * 587) + (col.b * 114)) / 1000;
            return (yiq >= 128) ? "#222222" : "#eeeeee";
        }
    })(stackgraph || (stackgraph = {}));
    function formatNum(v, places, sig, always) {
        if (places === void 0) { places = 6; }
        if (sig === void 0) { sig = false; }
        if (always === void 0) { always = false; }
        var pl = sig ? (v < 10 ? 1 : 0) : places;
        return (v % 1 == 0 && !always) ? v.toString() : v.toFixed(pl);
    }
    var metadataSet = false;
    function setMetadata(state) {
        if (!metadataSet && state.metadata != null) {
            document.getElementById("profile-source").textContent = state.metadata["source"];
            document.getElementById("profile-time").textContent = state.metadata["time"];
            document.title = "Profile for " + state.metadata["source"] + " generated at " + state.metadata["time"];
            metadataSet = true;
        }
    }
    var spinnerVisible = false;
    function toggleStreamingSpinner(state) {
        var elt = document.getElementById("progress");
        if (state.streaming && !spinnerVisible) {
            elt.style.display = "block";
            spinnerVisible = true;
        }
        else if (!state.streaming && spinnerVisible) {
            elt.style.display = "none";
            spinnerVisible = false;
        }
    }
    var resizing = false;
    function windowResizeCallback() {
        if (!resizing) {
            resizing = true;
            stackgraph.windowResizeCallback();
            window.setTimeout(function () {
                resizing = false;
            }, 50);
        }
    }
    // make a color scheme reflecting that scores > 4 are "very bad"
    function makeScoreColorScheme(colors) {
        var cs = d3.interpolateRgbBasis(colors);
        return function (x) {
            if (x > 4.0)
                return colors[colors.length - 1];
            return cs(x / 4.0);
        };
    }
    function bindHelpEventHandlers() {
        var helps = document.querySelectorAll(".help");
        for (var i = 0; i < helps.length; i++) {
            var elt = helps[i];
            elt.addEventListener("mouseover", function (evt) { return tooltip.showWithDelay("", evt.target, "top", 100); });
            elt.addEventListener("mouseout", function (evt) { return tooltip.hide(); });
        }
    }
    function init() {
        // set up tooltips
        tooltip.init();
        bindHelpEventHandlers();
        // set up analysis
        analysis.init();
        // initialize UI components
        stackgraph.initStackGraph();
        calltable.initCallTable();
        // set up UI callbacks for data state changes
        data.registerUpdateCallback(setMetadata);
        data.registerUpdateCallback(toggleStreamingSpinner);
        // receive all the data
        data.readyForData();
        // set initial widths correctly now that data is rendered
        windowResizeCallback();
        window.addEventListener("resize", windowResizeCallback);
    }
    document.addEventListener("DOMContentLoaded", init);
})(profile || (profile = {}));
//# sourceMappingURL=profile.js.map