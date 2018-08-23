var analysis;
(function (analysis) {
    var options = {
        aggregate: false,
        signatures: false,
        collapseRosette: false,
        collapseSolver: false,
        columns: [],
        contextDepth: 0,
        histogramBins: 100
    };
    var analysisCallbacks = [];
    var zoomCallbacks = [];
    var columnDenominators = {};
    var currentState = null;
    var currentZoom = null;
    // init
    function init() {
        data.registerUpdateCallback(receiveDataCallback);
    }
    analysis.init = init;
    function registerAnalysisCallback(cb) {
        analysisCallbacks.push(cb);
    }
    analysis.registerAnalysisCallback = registerAnalysisCallback;
    function registerZoomCallback(cb) {
        zoomCallbacks.push(cb);
    }
    analysis.registerZoomCallback = registerZoomCallback;
    function setAggregate(x) {
        options.aggregate = x;
        refresh();
    }
    analysis.setAggregate = setAggregate;
    function setColumns(x) {
        options.columns = x;
        refresh();
    }
    analysis.setColumns = setColumns;
    function setContextDepth(x) {
        options.contextDepth = x;
        refresh();
    }
    analysis.setContextDepth = setContextDepth;
    function setHistogramBins(x) {
        options.histogramBins = x;
        refresh();
    }
    analysis.setHistogramBins = setHistogramBins;
    function setSignatures(x) {
        options.signatures = x;
        refresh();
    }
    analysis.setSignatures = setSignatures;
    function setCollapseRosette(x) {
        options.collapseRosette = x;
        refresh();
    }
    analysis.setCollapseRosette = setCollapseRosette;
    function setCollapseSolver(x) {
        options.collapseSolver = x;
        refresh();
    }
    analysis.setCollapseSolver = setCollapseSolver;
    function refresh() {
        if (currentState)
            receiveDataCallback(currentState);
    }
    analysis.refresh = refresh;
    function receiveDataCallback(d) {
        // save the state for reuse when options are changed
        currentState = d;
        // which data to actually use
        var root = d.root;
        if (options.collapseRosette) {
            root = collapseRosetteCalls(root);
        }
        if (options.collapseSolver) {
            root = collapseSolverTime(root, d.solverCalls);
        }
        // compute scores for the data
        var maxScore = computeScores(root);
        // compute rows for the table
        var rows = computeTableRows(root);
        if (!currentZoom) {
            currentZoom = root;
        }
        var a = {
            root: root,
            solverCalls: d.solverCalls,
            rows: rows,
            currentZoom: currentZoom,
            aggregate: options.aggregate,
            maxScore: maxScore
        };
        for (var _i = 0, analysisCallbacks_1 = analysisCallbacks; _i < analysisCallbacks_1.length; _i++) {
            var cb = analysisCallbacks_1[_i];
            cb(a);
        }
    }
    function zoomTo(root) {
        currentZoom = root;
        for (var _i = 0, zoomCallbacks_1 = zoomCallbacks; _i < zoomCallbacks_1.length; _i++) {
            var cb = zoomCallbacks_1[_i];
            cb(currentZoom);
        }
    }
    analysis.zoomTo = zoomTo;
    function computeScores(root) {
        if (root === null)
            return 0.0;
        // first pass: compute denominators
        var maxValues = {};
        var nodes = [root];
        while (nodes.length > 0) {
            var n = nodes.pop();
            for (var _i = 0, _a = options.columns; _i < _a.length; _i++) {
                var c = _a[_i];
                if (n.hasOwnProperty(c.type)) {
                    var k = c.type + ":" + c.column;
                    maxValues[k] = Math.max(maxValues[k] || 0, n[c.type][c.column] || 0);
                }
            }
            for (var _b = 0, _c = n.children; _b < _c.length; _b++) {
                var c = _c[_b];
                nodes.push(c);
            }
        }
        // second pass: compute scores for each node
        nodes.push(root);
        var maxScore = 0;
        while (nodes.length > 0) {
            var n = nodes.pop();
            var score = 0.0;
            for (var _d = 0, _e = options.columns; _d < _e.length; _d++) {
                var c = _e[_d];
                if (c.score && n.hasOwnProperty(c.type)) {
                    var k = c.type + ":" + c.column;
                    if (maxValues[k] > 0) {
                        score += (n[c.type][c.column] || 0) / maxValues[k];
                    }
                }
            }
            n.score = score;
            if (score > maxScore) {
                maxScore = score;
            }
            for (var _f = 0, _g = n.children; _f < _g.length; _f++) {
                var c = _g[_f];
                nodes.push(c);
            }
        }
        return maxScore;
    }
    // compute scores for aggregated table rows
    function updateScoresForRows(rows) {
        if (rows.length == 0)
            return rows;
        // first pass: compute denominators
        var maxValues = [];
        for (var _i = 0, rows_1 = rows; _i < rows_1.length; _i++) {
            var r = rows_1[_i];
            for (var i = 0; i < r.columns.length; i++) {
                if (r.columns[i] >= (maxValues[i] || 0)) {
                    maxValues[i] = r.columns[i];
                }
            }
        }
        // second pass: compute scores for each row
        for (var _a = 0, rows_2 = rows; _a < rows_2.length; _a++) {
            var r = rows_2[_a];
            var score = 0.0;
            for (var i = 0; i < r.columns.length; i++) {
                if (options.columns[i].score && maxValues[i] > 0) {
                    score += r.columns[i] / maxValues[i];
                }
            }
            r.score = score;
        }
    }
    // get the key used for aggregating nodes together according to context
    function getAggregateKeyForNode(node) {
        var context = options.contextDepth;
        var key = node.name + "(" + node.source + ")";
        while (context-- != 0 && node.parent) {
            node = node.parent;
            key = key + "\\" + node.name + "(" + node.source + ")";
        }
        if (options.signatures) {
            key = (node.inputs["signature"] || []).join("->") + "|" +
                (node.outputs["signature"] || []).join("->") + "|" +
                key;
        }
        return key;
    }
    // compute the analysis rows by aggregating and scoring them
    function computeTableRows(root) {
        if (root === null)
            return [];
        if (options.aggregate) {
            // group rows by the aggregate key (wrt context)
            var nodes = [root];
            var ctxs = {};
            while (nodes.length > 0) {
                var n = nodes.pop();
                if (n) {
                    var k = getAggregateKeyForNode(n);
                    if (!ctxs.hasOwnProperty(k))
                        ctxs[k] = [];
                    ctxs[k].push(n);
                    for (var _i = 0, _a = n.children; _i < _a.length; _i++) {
                        var c = _a[_i];
                        nodes.push(c);
                    }
                }
            }
            // create row for each node
            var allRows = [];
            for (var k in ctxs) {
                var rows = ctxs[k];
                if (rows.length > 0) {
                    var first = rows[0];
                    // compute the row's data as the total within
                    var maxScore = 0.0;
                    var totalValues = {};
                    for (var _b = 0, rows_3 = rows; _b < rows_3.length; _b++) {
                        var n = rows_3[_b];
                        for (var _c = 0, _d = options.columns; _c < _d.length; _c++) {
                            var c = _d[_c];
                            if (n.hasOwnProperty(c.type)) {
                                var k_1 = c.type + ":" + c.column;
                                totalValues[k_1] = (totalValues[k_1] || 0) + (n[c.type][c.column] || 0);
                            }
                        }
                        maxScore = Math.max(maxScore, n.score);
                    }
                    var columns = [];
                    for (var _e = 0, _f = options.columns; _e < _f.length; _e++) {
                        var k_2 = _f[_e];
                        columns.push(totalValues[k_2.type + ":" + k_2.column]);
                    }
                    var row = {
                        function: first.name,
                        node: first,
                        allNodes: rows,
                        score: maxScore,
                        columns: columns
                    };
                    allRows.push(row);
                }
            }
            updateScoresForRows(allRows); // this should really be an option
            return allRows;
        }
        else {
            // create a row for each call
            var nodes = [root];
            var rows = [];
            while (nodes.length > 0) {
                var n = nodes.pop();
                if (!n)
                    continue;
                var values = {};
                for (var _g = 0, _h = options.columns; _g < _h.length; _g++) {
                    var c = _h[_g];
                    if (n.hasOwnProperty(c.type)) {
                        var k = c.type + ":" + c.column;
                        values[k] = n[c.type][c.column] || 0;
                    }
                }
                var columns = [];
                for (var _j = 0, _k = options.columns; _j < _k.length; _j++) {
                    var k = _k[_j];
                    columns.push(values[k.type + ":" + k.column]);
                }
                var row = {
                    function: n.name,
                    node: n,
                    allNodes: [n],
                    score: n.score,
                    columns: columns
                };
                rows.push(row);
                for (var _l = 0, _m = n.children; _l < _m.length; _l++) {
                    var c = _m[_l];
                    nodes.push(c);
                }
            }
            return rows;
        }
    }
    function collapseRosetteCalls(root) {
        var rec = function (node) {
            var newExcl = undefined;
            var newChildren = [];
            var modified = false;
            for (var _i = 0, _a = node.children; _i < _a.length; _i++) {
                var c = _a[_i];
                var newC = rec(c); // recurse to collapse children
                if (newC.name[0] == "@") {
                    if (typeof newExcl === "undefined") {
                        newExcl = {};
                        for (var _b = 0, _c = Object.keys(node.excl); _b < _c.length; _b++) {
                            var k = _c[_b];
                            newExcl[k] = node.excl[k];
                        }
                    }
                    for (var _d = 0, _e = Object.keys(newC.excl); _d < _e.length; _d++) {
                        var k = _e[_d];
                        newExcl[k] = (newExcl[k] || 0) + newC.excl[k]; // add all c's children
                    }
                    for (var _f = 0, _g = newC.children; _f < _g.length; _f++) {
                        var cc = _g[_f];
                        newChildren.push(cc);
                    }
                    modified = true;
                }
                else {
                    if (newC !== c) {
                        modified = true;
                    }
                    newChildren.push(newC); // recurse
                }
            }
            if (modified) {
                var newNode = {};
                for (var _h = 0, _j = Object.keys(node); _h < _j.length; _h++) {
                    var k = _j[_h];
                    newNode[k] = node[k];
                }
                if (typeof newExcl !== "undefined") {
                    newNode.excl = newExcl;
                }
                newNode.children = newChildren;
                return newNode;
            }
            else {
                return node;
            }
        };
        var newRoot = rec(root);
        return newRoot;
    }
    // remove all solver time from exclusive time
    function collapseSolverTime(root, solverCalls) {
        var rec = function (node) {
            var exclDt = 0;
            var newChildren = [];
            var modified = false;
            var start = node.start;
            // do the spaces before each child
            for (var _i = 0, _a = node.children; _i < _a.length; _i++) {
                var c = _a[_i];
                var finish_1 = c.start;
                for (var _b = 0, solverCalls_1 = solverCalls; _b < solverCalls_1.length; _b++) {
                    var sc = solverCalls_1[_b];
                    var scfinish = typeof sc.finish === "undefined" ? root.finish : sc.finish;
                    if (scfinish < start)
                        continue;
                    if (finish_1 < sc.start)
                        break; // todo make not quadratic
                    var delta = Math.min(finish_1, scfinish) - Math.max(start, sc.start);
                    exclDt += delta;
                }
                var ret = rec(c);
                newChildren.push(ret);
                if (ret !== c)
                    modified = true;
                start = c.finish;
            }
            // do the space between last child and my end
            var finish = node.finish;
            for (var _c = 0, solverCalls_2 = solverCalls; _c < solverCalls_2.length; _c++) {
                var sc = solverCalls_2[_c];
                var scfinish = typeof sc.finish === "undefined" ? root.finish : sc.finish;
                if (scfinish < start)
                    continue;
                if (finish < sc.start)
                    break;
                var delta = Math.min(finish, scfinish) - Math.max(start, sc.start);
                exclDt += delta;
            }
            if (exclDt > 0 || modified) {
                var newNode = {};
                for (var _d = 0, _e = Object.keys(node); _d < _e.length; _d++) {
                    var k = _e[_d];
                    newNode[k] = node[k];
                }
                var newExcl = {};
                for (var _f = 0, _g = Object.keys(node.excl); _f < _g.length; _f++) {
                    var k = _g[_f];
                    newExcl[k] = node.excl[k];
                }
                newNode.excl = newExcl;
                newNode.excl["time"] -= exclDt;
                newNode.children = newChildren;
                node = newNode;
            }
            return node;
        };
        return rec(root);
    }
})(analysis || (analysis = {}));
//# sourceMappingURL=analysis.js.map