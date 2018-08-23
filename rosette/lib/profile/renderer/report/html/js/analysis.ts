namespace analysis {
    import CallNode = data.CallNode;
    import SolverCall = data.SolverCall;

    type AnalysisCallback = (d: ProfileData) => void;
    type ZoomCallback = (d: CallNode) => void;

    export interface ProfileData {
        root: CallNode;
        solverCalls: SolverCall[];
        rows: AnalysisRow[];
        currentZoom: CallNode;
        aggregate: boolean;
        maxScore: number;
    }

    export interface AnalysisRow {
        function: string;
        node: CallNode;
        allNodes: CallNode[];
        score: number;
        columns: number[];
    }

    export interface AnalysisColumn {
        type: string;
        column: string;
        name: string;
        score: boolean;
        description?: string;
    }

    var options = {
        aggregate: false,
        signatures: false,
        collapseRosette: false,
        collapseSolver: false,
        columns: <AnalysisColumn[]>[],
        contextDepth: 0,
        histogramBins: 100
    };

    var analysisCallbacks: AnalysisCallback[] = [];
    var zoomCallbacks: ZoomCallback[] = [];
    var columnDenominators: Map<number> = {};
    var currentState: data.ProfileState = null;
    var currentZoom: CallNode = null;

    // init
    export function init() {
        data.registerUpdateCallback(receiveDataCallback);
    }

    export function registerAnalysisCallback(cb: AnalysisCallback): void {
        analysisCallbacks.push(cb);
    }

    export function registerZoomCallback(cb: ZoomCallback): void {
        zoomCallbacks.push(cb);
    }

    export function setAggregate(x: boolean): void {
        options.aggregate = x;
        refresh();
    }
    export function setColumns(x: AnalysisColumn[]): void {
        options.columns = x;
        refresh();
    }
    export function setContextDepth(x: number): void {
        options.contextDepth = x;
        refresh();
    }
    export function setHistogramBins(x: number): void {
        options.histogramBins = x;
        refresh();
    }
    export function setSignatures(x: boolean): void {
        options.signatures = x;
        refresh();
    }
    export function setCollapseRosette(x: boolean): void {
        options.collapseRosette = x;
        refresh();
    }
    export function setCollapseSolver(x: boolean): void {
        options.collapseSolver = x;
        refresh();
    }

    export function refresh(): void {
        if (currentState) receiveDataCallback(currentState);
    }

    function receiveDataCallback(d: data.ProfileState): void {
        // save the state for reuse when options are changed
        currentState = d;

        // which data to actually use
        let root: CallNode = d.root;
        if (options.collapseRosette) {
            root = collapseRosetteCalls(root);
        }
        if (options.collapseSolver) {
            root = collapseSolverTime(root, d.solverCalls);
        }

        // compute scores for the data
        let maxScore = computeScores(root);

        // compute rows for the table
        let rows = computeTableRows(root);

        if (!currentZoom) {
            currentZoom = root;
        }

        let a: ProfileData = {
            root: root,
            solverCalls: d.solverCalls,
            rows: rows,
            currentZoom: currentZoom,
            aggregate: options.aggregate,
            maxScore: maxScore
        };

        for (let cb of analysisCallbacks) {
            cb(a);
        }
    }


    export function zoomTo(root: CallNode) {
        currentZoom = root;
        for (let cb of zoomCallbacks) {
            cb(currentZoom);
        }
    }


    function computeScores(root: CallNode): number {
        if (root === null) return 0.0;

        // first pass: compute denominators
        let maxValues: Map<number> = {};
        let nodes = [root];
        while (nodes.length > 0) {
            let n = nodes.pop()!;
            for (let c of options.columns) {
                if (n.hasOwnProperty(c.type)) {
                    let k = c.type + ":" + c.column;
                    maxValues[k] = Math.max(maxValues[k] || 0, n[c.type][c.column] || 0);
                }
            }
            for (let c of n.children) nodes.push(c);
        }
        // second pass: compute scores for each node
        nodes.push(root);
        let maxScore = 0;
        while (nodes.length > 0) {
            let n = nodes.pop()!;
            let score = 0.0;
            for (let c of options.columns) {
                if (c.score && n.hasOwnProperty(c.type)) {
                    let k = c.type + ":" + c.column;
                    if (maxValues[k] > 0) {
                        score += (n[c.type][c.column] || 0) / maxValues[k];
                    }
                }
            }
            n.score = score;
            if (score > maxScore) {
                maxScore = score;
            }
            for (let c of n.children) nodes.push(c);
        }
        return maxScore;
    }


    // compute scores for aggregated table rows
    function updateScoresForRows(rows: AnalysisRow[]) {
        if (rows.length == 0) return rows;

        // first pass: compute denominators
        let maxValues: number[] = [];
        for (let r of rows) {
            for (let i = 0; i < r.columns.length; i++) {
                if (r.columns[i] >= (maxValues[i] || 0)) {
                    maxValues[i] = r.columns[i];
                }
            }
        }

        // second pass: compute scores for each row
        for (let r of rows) {
            let score = 0.0;
            for (let i = 0; i < r.columns.length; i++) {
                if (options.columns[i].score && maxValues[i] > 0) {
                    score += r.columns[i] / maxValues[i];
                }
            }
            r.score = score;
        }
    }


    // get the key used for aggregating nodes together according to context
    function getAggregateKeyForNode(node: CallNode): string {
        var context = options.contextDepth;
        var key = node.name + "(" + node.source + ")";
        while (context-- != 0 && node.parent) {
            node = node.parent;
            key = key + "\\" + node.name + "(" + node.source + ")";
        }
        if (options.signatures) {
            key = (node.inputs["signature"]  || []).join("->") + "|" +
                  (node.outputs["signature"] || []).join("->") + "|" +
                  key;
        }
        return key;
    }


    // compute the analysis rows by aggregating and scoring them
    function computeTableRows(root: CallNode): AnalysisRow[] {
        if (root === null) return [];

        if (options.aggregate) {
            // group rows by the aggregate key (wrt context)
            let nodes = [root];
            let ctxs: Map<CallNode[]> = {};
            while (nodes.length > 0) {
                let n = nodes.pop()!;
                if (n) {
                    let k = getAggregateKeyForNode(n);
                    if (!ctxs.hasOwnProperty(k)) ctxs[k] = [];
                    ctxs[k].push(n);
                    for (let c of n.children) nodes.push(c);
                }
            }
            // create row for each node
            let allRows: AnalysisRow[] = [];
            for (let k in ctxs) {
                let rows = ctxs[k];
                if (rows.length > 0) {
                    let first = rows[0]!;
                    // compute the row's data as the total within
                    let maxScore = 0.0;
                    let totalValues: Map<number> = {};
                    for (let n of rows) {
                        for (let c of options.columns) {
                            if (n.hasOwnProperty(c.type)) {
                                let k = c.type + ":" + c.column;
                                totalValues[k] = (totalValues[k] || 0) + (n[c.type][c.column] || 0);
                            }
                        }
                        maxScore = Math.max(maxScore, n.score);
                    }
                    let columns = [];
                    for (let k of options.columns) {
                        columns.push(totalValues[k.type + ":" + k.column]);
                    }
                    let row: AnalysisRow = {
                        function: first.name,
                        node: first,
                        allNodes: rows,
                        score: maxScore,
                        columns: columns
                    };
                    allRows.push(row);
                }
            }
            updateScoresForRows(allRows);  // this should really be an option
            return allRows;
        } else {
            // create a row for each call
            let nodes = [root];
            let rows: AnalysisRow[] = [];
            while (nodes.length > 0) {
                let n = nodes.pop()!;
                if (!n) continue;
                let values: Map<number> = {};
                for (let c of options.columns) {
                    if (n.hasOwnProperty(c.type)) {
                        let k = c.type + ":" + c.column;
                        values[k] = n[c.type][c.column] || 0;
                    }
                }
                let columns = [];
                for (let k of options.columns) {
                    columns.push(values[k.type + ":" + k.column]);
                }
                let row: AnalysisRow = {
                    function: n.name,
                    node: n,
                    allNodes: [n],
                    score: n.score,
                    columns: columns
                };
                rows.push(row);
                for (let c of n.children) nodes.push(c);
            }
            return rows;
        }
    }


    function collapseRosetteCalls(root: CallNode): CallNode {
        let rec = (node: CallNode): CallNode => {
            let newExcl = undefined;
            let newChildren = [];
            let modified = false;
            for (let c of node.children) {
                let newC = rec(c);  // recurse to collapse children
                if (newC.name[0] == "@") {
                    if (typeof newExcl === "undefined") {
                        newExcl = {};
                        for (let k of Object.keys(node.excl)) {
                            newExcl[k] = node.excl[k];
                        }
                    }
                    for (let k of Object.keys(newC.excl)) {
                        newExcl[k] = (newExcl[k] || 0) + newC.excl[k]; // add all c's children
                    }
                    for (let cc of newC.children) {
                        newChildren.push(cc);
                    }
                    modified = true;
                } else {
                    if (newC !== c) {
                        modified = true;
                    }
                    newChildren.push(newC);  // recurse
                }
            }
            if (modified) {
                let newNode = {};
                for (let k of Object.keys(node)) {
                    newNode[k] = node[k];
                }
                if (typeof newExcl !== "undefined") {
                    (<CallNode>newNode).excl = newExcl;
                }
                (<CallNode>newNode).children = newChildren;
                return <CallNode>newNode;
            } else {
                return node;
            }
        }
        let newRoot = rec(root);
        return newRoot;
    }


    // remove all solver time from exclusive time
    function collapseSolverTime(root: CallNode, solverCalls: SolverCall[]): CallNode {
        let rec = (node: CallNode) => {
            let exclDt = 0;
            let newChildren = [];
            let modified = false;
            let start = node.start;
            // do the spaces before each child
            for (let c of node.children) {
                let finish = c.start;
                for (let sc of solverCalls) {
                    let scfinish = typeof sc.finish === "undefined" ? root.finish : sc.finish;
                    if (scfinish < start) continue;
                    if (finish < sc.start) break;  // todo make not quadratic
                    let delta = Math.min(finish, scfinish) - Math.max(start, sc.start);
                    exclDt += delta;
                }
                let ret = rec(c);
                newChildren.push(ret);
                if (ret !== c) modified = true;
                start = c.finish;
            }
            // do the space between last child and my end
            let finish = node.finish;
            for (let sc of solverCalls) {
                let scfinish = typeof sc.finish === "undefined" ? root.finish : sc.finish;
                if (scfinish < start) continue;
                if (finish < sc.start) break;
                let delta = Math.min(finish, scfinish) - Math.max(start, sc.start);
                exclDt += delta;
            }
            if (exclDt > 0 || modified) {
                let newNode = {};
                for (let k of Object.keys(node)) {
                    newNode[k] = node[k];
                }
                let newExcl = {};
                for (let k of Object.keys(node.excl)) {
                    newExcl[k] = node.excl[k];
                }
                (<CallNode>newNode).excl = newExcl;
                (<CallNode>newNode).excl["time"] -= exclDt;
                (<CallNode>newNode).children = newChildren;
                node = (<CallNode>newNode);
            }
            return node;
        }

        return rec(root);
    }
}