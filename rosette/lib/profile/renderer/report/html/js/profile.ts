declare var d3;
declare var Tablesort;

interface Map<T> { [key: string]: T}

namespace profile {
    import CallNode = data.CallNode;
    import ProfileState = data.ProfileState;

    namespace calltable {
        const scoreColumns: analysis.AnalysisColumn[] = [
            {type: "excl", column: "time", name: "Time (ms)", description: "Total time spent in this function (but not in descendent calls)", score: true},
            {type: "excl", column: "term-count", name: "Term Count", description: "Number of symbolic terms created", score: true},
            {type: "excl", column: "unused-terms", name: "Unused Terms", description: "Number of symbolic terms created that were never used for solving", score: true},
            {type: "excl", column: "union-size", name: "Union Size", description: "Total number of branches in all symbolic unions created", score: true},
            {type: "excl", column: "merge-cases", name: "Merge Cases", description: "Number of branches used during merging", score: true},
        ];

        const DOM_ROW_KEY = "symproRowObject";

        const PRUNE_SCORE_FACTOR = 0.01; // < 1% of max score = pruned

        const colorScheme = makeScoreColorScheme(["#000000", "#FD893C", "#D9002C"]);

        var tableSorter;
        var contextDepth = 0;
        var useSignatures = false;
        var useCallsites = false;
        var pruneSmallRows = true;
        var idToRow: Map<HTMLTableRowElement> = {};

        export function initCallTable() {
            renderTableHeaders();
            analysis.setAggregate(true);
            analysis.setColumns(scoreColumns);
            analysis.registerAnalysisCallback(receiveData);
        }

        function renderTableHeaders(): void {
            let thead = document.querySelector("#calltable thead");
            let tr = document.createElement("tr");

            makeCell("Function", tr, "th");

            let scoreCell = makeCell("Score", tr, "th");
            scoreCell.className = "sort-default score";
            scoreCell.id = "calltable-score-header";

            let keys = [];
            for (let i = 0; i < scoreColumns.length; i++) {
                let c = scoreColumns[i];
                let cell = makeCell(c.name, tr, "th");
                if (c.description) {
                    cell.dataset["title"] = c.description;
                    cell.addEventListener("mouseover", (evt) => tooltip.showWithDelay("", <HTMLElement>evt.target, "top", 100));
                    cell.addEventListener("mouseout", (evt) => tooltip.hide());
                }
                let checkbox = document.createElement("input");
                checkbox.type = "checkbox";
                checkbox.checked = true;
                checkbox.className = "score-checkbox";
                checkbox.value = i.toString();
                checkbox.addEventListener("change", scoreSelectCallback);
                checkbox.addEventListener("click", (evt) => evt.stopPropagation()); // prevent tablesorter from firing
                cell.insertAdjacentElement("beforeend", checkbox);
            }

            thead.insertAdjacentElement("beforeend", tr);

            // set up configuration controls
            let config = <HTMLTableHeaderCellElement>document.getElementById("calltable-config")!;
            // aggregate checkbox
            let agg = <HTMLInputElement>config.querySelector("#calltable-aggregate");
            agg.checked = true;
            agg.addEventListener("change", configChangeCallback);
            // context slider
            let ctx = <HTMLInputElement>config.querySelector("#calltable-context");
            ctx.value = "0";
            let isIE = !!navigator.userAgent.match(/Trident/g) || !!navigator.userAgent.match(/MSIE/g);
            ctx.addEventListener(isIE ? "change" : "input", configChangeCallback);
            // context count
            let ctxn = config.querySelector("#calltable-context-n");
            ctxn.textContent = "0";
            // filter checkbox
            let fil = <HTMLInputElement>config.querySelector("#calltable-prune");
            fil.checked = true;
            fil.addEventListener("change", configChangeCallback);
            // collapse rosette checkbox
            let clr = <HTMLInputElement>config.querySelector("#calltable-collapse-rosette");
            clr.checked = false;
            clr.addEventListener("change", configChangeCallback);
            // collapse solver checkbox
            let cls = <HTMLInputElement>config.querySelector("#calltable-collapse-solver");
            cls.checked = false;
            cls.addEventListener("change", configChangeCallback);
            // signature checkbox
            let sig = <HTMLInputElement>config.querySelector("#calltable-signature");
            sig.checked = false;
            sig.addEventListener("change", configChangeCallback);
            // callsites checkbox
            let css = <HTMLInputElement>config.querySelector("#calltable-callsites");
            css.checked = false;
            css.parentElement.style.display = "none";
            css.addEventListener("change", configChangeCallback);
            // score checkbox
            let sco = <HTMLInputElement>config.querySelector("#calltable-show-scoreboxes");
            sco.checked = false;
            sco.addEventListener("change", configChangeCallback);
            // more config
            let more = <HTMLDivElement>config.querySelector("#calltable-config-more");
            more.style.display = "none";
            let moreLink = <HTMLInputElement>config.querySelector("#calltable-config-toggle-more");
            moreLink.addEventListener("click", toggleMoreCallback);

            // attach event handler for table body
            let tbody = document.querySelector("#calltable tbody");
            tbody.addEventListener("mouseover", hoverCallback);
            tbody.addEventListener("mouseout", hoverCallback);

            tableSorter = new Tablesort(document.getElementById("calltable"), { descending: true });
        }

        function configChangeCallback(evt: Event) {
            let elt: HTMLInputElement = this;
            if (elt.id == "calltable-aggregate") {
                let lbl = document.getElementById("calltable-callsites").parentElement;
                lbl.style.display = (elt.checked ? "none" : "");
                analysis.setAggregate(elt.checked);
            } else if (elt.id == "calltable-context") {
                contextDepth = elt.value == elt.max ? -1 : parseInt(elt.value);
                let ctxn = document.getElementById("calltable-context-n");
                ctxn.textContent = contextDepth >= 0 ? elt.value : "∞";
                analysis.setContextDepth(contextDepth);
            } else if (elt.id == "calltable-prune") {
                pruneSmallRows = elt.checked;
                analysis.refresh();
            } else if (elt.id == "calltable-collapse-rosette") {
                analysis.setCollapseRosette(elt.checked);
            } else if (elt.id == "calltable-collapse-solver") {
                stackgraph.setCollapseSolverTime(elt.checked);
                analysis.setCollapseSolver(elt.checked);
            } else if (elt.id == "calltable-signature") {
                useSignatures = elt.checked;
                analysis.setSignatures(elt.checked);
            } else if (elt.id == "calltable-callsites") {
                useCallsites = elt.checked;
                analysis.refresh();
            } else if (elt.id == "calltable-show-scoreboxes") {
                let boxes = <NodeListOf<HTMLElement>>document.querySelectorAll("#calltable .score-checkbox");
                for (let i = 0; i < boxes.length; i++) {
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

        function toggleMoreCallback(evt: Event) {
            evt.preventDefault();
            let more = document.getElementById("calltable-config-more");
            let elt: HTMLAnchorElement = this;
            if (more.style.display == "none") {
                more.style.display = "block";
                this.textContent = "[Less]";
            } else {
                more.style.display = "none";
                this.textContent = "[More]";
            }
        }

        function hoverCallback(evt: Event) {
            if (evt.type == "mouseover") {
                let tgt = <HTMLElement>evt.target;
                while (tgt && tgt.tagName != "TR") tgt = tgt.parentElement;

                stackgraph.calltableHoverCallback(tgt[DOM_ROW_KEY].allNodes);
            }
            else {
                stackgraph.calltableHoverCallback([]);
            }
        }

        function scoreSelectCallback(evt: Event) {
            let elt: HTMLInputElement = this;
            let idx = parseInt(elt.value);
            scoreColumns[idx].score = elt.checked;
            analysis.setColumns(scoreColumns);
        }

        export function stackgraphHoverCallback(node: CallNode, enter: boolean) {
            let rows = document.querySelectorAll("#calltable tbody tr");
            for (let i = 0; i < rows.length; i++) {
                let row = rows[i];
                row.className = "";
            }
            if (enter) {
                let hiRow = idToRow[node.id];
                if (hiRow) {
                    hiRow.className = "calltable-highlight";
                }
            }
        }

        function receiveData(state: analysis.ProfileData) {
            renderTableRows(state.rows, state.aggregate, state.maxScore);
        }

        function renderPrettySource(source: string, cell: HTMLElement, iscallsite=false) {
            if (source) {
                let prettySource = getPrettySource(source);
                if (iscallsite) prettySource = "from " + prettySource;
                let sourceSpan = makeCell(prettySource, cell, "span");
                sourceSpan.className = "source";
                sourceSpan.title = source;
            }
        }

        function getPrettySource(source: string) {
            let sourceParts = source.split(":");
            if (sourceParts.length != 3) return source;
            let [file, line, col] = sourceParts;
            let fileParts = file.split("/");
            return fileParts[fileParts.length-1] + ":" + line;
        }

        function renderTableRows(rows: analysis.AnalysisRow[], aggregate: boolean, maxScore: number) {
            // remove old rows
            let tbody = document.querySelector("#calltable tbody");
            while (tbody.firstChild) tbody.removeChild(tbody.firstChild);
            idToRow = {};
    
            // create new rows
            for (let r of rows) {
                let row = document.createElement("tr");

                if (pruneSmallRows && r.score < maxScore*PRUNE_SCORE_FACTOR) {
                    continue;
                }

                // render the function name
                let nameCell = makeCell(r.function, row);
                nameCell.className = "name";
                if (useCallsites && !aggregate) {
                    renderPrettySource(r.node.callsite, nameCell, true);
                } else {
                    renderPrettySource(r.node.source, nameCell);
                }
                if (aggregate) {
                    let txt = r.allNodes.length > 1 ? formatNum(r.allNodes.length) + " calls" : "1 call";
                    let numCallsSpan = makeCell(txt, nameCell, "span");
                    numCallsSpan.className = "numcalls";
                }

                // maybe render the signature
                if (useSignatures) {
                    let inputs: string[] = r.node.inputs["signature"] || [];
                    let outputs: string[] = r.node.outputs["signature"] || [];
                    let istr = inputs.map((s) => s[0].toUpperCase()).join("");
                    let ostr = outputs.map((s) => s[0].toUpperCase()).join("");
                    let span = makeCell(istr + "→" + ostr, nameCell, "span");
                    span.className = "signature";
                }

                // render the call context if requested
                if (contextDepth != 0) {
                    let contextList = document.createElement("ul");
                    contextList.className = "context-list";
                    let n = contextDepth, curr = r.node;
                    if (n == -1) {  // if "infinite", collapse recursion
                        let count = 0;
                        while (curr = curr.parent) {
                            if (curr.parent && curr.parent.name == curr.name && curr.parent.source == curr.source) {
                                count += 1;
                            } else {
                                let name = curr.name + (count > 0 ? " ×" + (count+1) : "");
                                let li = makeCell(name, contextList, "li");
                                count = 0;
                                renderPrettySource(curr.source, li);
                            }
                        }
                    } else {
                        while (n-- != 0 && (curr = curr.parent)) {
                            let li = makeCell(curr.name, contextList, "li");
                            renderPrettySource(curr.source, li);
                        }
                    }
                    nameCell.insertAdjacentElement("beforeend", contextList);
                }
                
                // score cell
                let scoreBar = document.createElement("div");
                scoreBar.className = "scorebar";
                scoreBar.style.width = (r.score/scoreColumns.length * 100) + "%";
                scoreBar.style.backgroundColor = colorScheme(r.score);
                let scoreBarCell = document.createElement("div");
                scoreBarCell.className = "scorecell-bar";
                scoreBarCell.insertAdjacentElement("beforeend", scoreBar);
                let scoreSpan = document.createElement("span");
                scoreSpan.textContent = formatNum(r.score, 1, false, true);
                scoreSpan.style.color = colorScheme(r.score);
                let scoreSpanCell = document.createElement("div");
                scoreSpanCell.className = "scorecell-score";
                scoreSpanCell.insertAdjacentElement("beforeend", scoreSpan);
                let scoreCell = makeCell("", row);
                scoreCell.dataset["sort"] = r.score.toFixed(16);
                scoreCell.insertAdjacentElement("beforeend", scoreSpanCell);
                scoreCell.insertAdjacentElement("beforeend", scoreBarCell);

                // data columns
                for (let k of r.columns) {
                    makeCell(formatNum(k, 0), row);
                }

                // attach row object to the row
                row[DOM_ROW_KEY] = r;

                // record IDs
                for (let n of r.allNodes) {
                    idToRow[n.id] = row;
                }

                tbody.insertAdjacentElement("beforeend", row);
            }
    
            // refresh the sort
            tableSorter.refresh();
        }
    
        function makeCell(str: string, row: HTMLElement, type: string = "td"): HTMLElement {
            let elt = document.createElement(type);
            elt.textContent = str;
            row.insertAdjacentElement('beforeend', elt);
            return elt;
        }
    }

    namespace stackgraph {
        var stackgraph: d3_stackgraph.StackGraph;
        const colorScheme = makeScoreColorScheme(["#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#bd0026"]);
        var solverHighlightColor = "#E3F2FF";
        var lastWidth = 0;
        var useDiscontinuities = false;

        export function initStackGraph() {
            // build stackgraph
            const STACK_WIDTH = document.getElementById("stackgraph")!.clientWidth;
            const STACK_HEIGHT = 270;
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

        function makeHighlightList(state: analysis.ProfileData) {
            let ret = [];
            for (let call of state.solverCalls) {
                let finish = typeof call.finish === "undefined" ? state.root.finish : call.finish;
                let dt = formatNum(finish - call.start, 1);
                let summary = "";
                if (call.type == data.SolverCallType.SOLVE) {
                    let sat = typeof call.sat === "undefined" ? " (pending)" : (call.sat ? " (SAT)" : " (UNSAT)");
                    summary = `Solver call${sat}: ${dt}ms`;
                } else if (call.type == data.SolverCallType.ENCODE) {
                    summary = `Solver encoding: ${dt}ms`;
                } else if (call.type == data.SolverCallType.FINITIZE) {
                    summary = `Solver finitization: ${dt}ms`;
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

        function makeDiscontinuityList(state: analysis.ProfileData) {
            let ret = []
            for (let call of state.solverCalls) {
                let finish = typeof call.finish === "undefined" ? state.root.finish : call.finish;
                ret.push([call.start, finish]);
            }
            return ret;
        }

        function receiveData(state: analysis.ProfileData) {
            if (state.root) stackgraph.data(state.root);
            stackgraph.highlights(makeHighlightList(state));
            let discs = useDiscontinuities ? makeDiscontinuityList(state) : [];
            stackgraph.discontinuities(discs);
            stackgraph.render();
        }

        function receiveZoom(node: CallNode) {
            stackgraph.zoom(node);
        }

        function clickCallback(node: CallNode): void {
            analysis.zoomTo(node);
        }

        export function windowResizeCallback() {  // caller should handle debouncing
            let width = document.getElementById("stackgraph")!.clientWidth;
            if (width != lastWidth) {
                stackgraph.width(width).render();
                lastWidth = width;
            }
        }

        export function calltableHoverCallback(nodes: CallNode[]): void {
            stackgraph.highlightData(nodes);
        }

        export function setCollapseSolverTime(use: boolean) {
            useDiscontinuities = use;
        }

        function hoverCallback(node: CallNode, enter: boolean): void {
            calltable.stackgraphHoverCallback(node, enter);
        }

        function nodeColorCallback(node: CallNode): string {
            return colorScheme(node.score);
        }
        function nodeTextColorCallback(node: CallNode): string {
            let col = d3.color(colorScheme(node.score)).rgb();
            let yiq = ((col.r*299) + (col.g*587) + (col.b*114)) / 1000;
            return (yiq >= 128) ? "#222222" : "#eeeeee";
        }
    }


    function formatNum(v: number, places: number = 6, sig: boolean = false, always: boolean = false): string {
        let pl = sig ? (v < 10 ? 1 : 0) : places;
        return (v % 1 == 0 && !always) ? v.toString() : v.toFixed(pl);
    }


    var metadataSet = false;
    function setMetadata(state: ProfileState) {
        if (!metadataSet && state.metadata != null) {
            document.getElementById("profile-source").textContent = state.metadata["source"];
            document.getElementById("profile-time").textContent = state.metadata["time"];
            document.title = "Profile for " + state.metadata["source"] + " generated at " + state.metadata["time"];
            metadataSet = true;
        }
    }


    var spinnerVisible = false;
    function toggleStreamingSpinner(state: ProfileState) {
        let elt = document.getElementById("progress");
        if (state.streaming && !spinnerVisible) {
            elt.style.display = "block";
            spinnerVisible = true;
        } else if (!state.streaming && spinnerVisible) {
            elt.style.display = "none";
            spinnerVisible = false;
        }
    }


    var resizing = false;
    function windowResizeCallback() {
        if (!resizing) {
            resizing = true;
            stackgraph.windowResizeCallback();
            window.setTimeout(() => {
                resizing = false;
            }, 50);
        }
    }


    // make a color scheme reflecting that scores > 4 are "very bad"
    function makeScoreColorScheme(colors: string[]) {
        let cs = d3.interpolateRgbBasis(colors);
        return function(x) {
            if (x > 4.0) return colors[colors.length-1];
            return cs(x/4.0);
        };
    }


    function bindHelpEventHandlers() {
        var helps = document.querySelectorAll(".help");
        for (var i = 0; i < helps.length; i++) {
            let elt = helps[i];
            elt.addEventListener("mouseover", (evt) => tooltip.showWithDelay("", <HTMLElement>evt.target, "top", 100));
            elt.addEventListener("mouseout", (evt) => tooltip.hide());
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
}