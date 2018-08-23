var data;
(function (data_1) {
    var SolverCallType;
    (function (SolverCallType) {
        SolverCallType[SolverCallType["SOLVE"] = 0] = "SOLVE";
        SolverCallType[SolverCallType["ENCODE"] = 1] = "ENCODE";
        SolverCallType[SolverCallType["FINITIZE"] = 2] = "FINITIZE";
    })(SolverCallType = data_1.SolverCallType || (data_1.SolverCallType = {}));
    ;
    // buffer messages until document ready
    var bufferedMessages = [];
    var ready = false;
    var currentState = {
        metadata: null,
        root: null,
        current: null,
        idToNode: {},
        zeroMetrics: null,
        solverCalls: [],
        streaming: false
    };
    var updateCallbacks = [];
    function registerUpdateCallback(cb) {
        updateCallbacks.push(cb);
    }
    data_1.registerUpdateCallback = registerUpdateCallback;
    function unregisterUpdateCallback(cb) {
        var i = updateCallbacks.indexOf(cb);
        if (i > -1)
            updateCallbacks.splice(i, 1);
    }
    data_1.unregisterUpdateCallback = unregisterUpdateCallback;
    function readyForData() {
        if (!ready) {
            ready = true;
            receiveData(bufferedMessages);
            bufferedMessages = [];
        }
    }
    data_1.readyForData = readyForData;
    function diffMetrics(p1, p2) {
        var ret = {};
        for (var k in p1) {
            if (p2.hasOwnProperty(k)) {
                ret[k] = p2[k] - p1[k];
            }
        }
        return ret;
    }
    function exclMetrics(incl, children) {
        var ret = {};
        for (var k in incl) {
            ret[k] = incl[k];
        }
        for (var _i = 0, children_1 = children; _i < children_1.length; _i++) {
            var c = children_1[_i];
            for (var k in incl) {
                ret[k] -= c.incl[k] || 0;
            }
        }
        return ret;
    }
    var messages;
    (function (messages) {
        function receiveMetadataMessage(msg) {
            delete msg["type"];
            currentState.metadata = msg;
        }
        messages.receiveMetadataMessage = receiveMetadataMessage;
        // update the ProfileState using data from the profile message
        function receiveCallgraphMessage(msg) {
            var evts = msg["events"];
            if (evts.length == 0)
                return;
            if (!currentState.zeroMetrics) {
                currentState.zeroMetrics = evts[0].metrics;
            }
            for (var _i = 0, evts_1 = evts; _i < evts_1.length; _i++) {
                var e = evts_1[_i];
                if (e.type == "ENTER") {
                    if (!currentState.current && currentState.root) {
                        console.error("multiple root procedure calls");
                        return;
                    }
                    // e has fields:
                    //   id, function, location, inputs, metrics
                    var dm = diffMetrics(currentState.zeroMetrics, e.metrics);
                    var node = {
                        id: e.id,
                        name: e.function,
                        callsite: e.callsite,
                        source: e.source,
                        start: dm.time,
                        finish: null,
                        startMetrics: dm,
                        finishMetrics: null,
                        isFinished: false,
                        inputs: e.inputs,
                        outputs: {},
                        children: [],
                        parent: currentState.current,
                        incl: {},
                        excl: {},
                        score: 0
                    };
                    if (currentState.current)
                        currentState.current.children.push(node);
                    if (!currentState.root) // might be the first call
                        currentState.root = node;
                    currentState.current = node;
                    currentState.idToNode[e.id] = node;
                }
                else if (e.type == "EXIT") {
                    if (!currentState.current) {
                        console.error("unbalanced EXIT event");
                    }
                    // e has fields:
                    //   outputs, metrics
                    var dm = diffMetrics(currentState.zeroMetrics, e.metrics);
                    currentState.current.finish = dm.time;
                    currentState.current.finishMetrics = dm;
                    currentState.current.outputs = e.outputs;
                    currentState.current.isFinished = true;
                    currentState.current.incl = diffMetrics(currentState.current.startMetrics, dm);
                    currentState.current.excl = exclMetrics(currentState.current.incl, currentState.current.children);
                    currentState.current = currentState.current.parent;
                }
            }
            // set fake finish times and metrics for unclosed nodes
            var fakeFinishMetrics = diffMetrics(currentState.zeroMetrics, evts[evts.length - 1].metrics);
            var fakeFinishTime = fakeFinishMetrics.time;
            var curr = currentState.current;
            while (curr) {
                if (!curr.isFinished) {
                    curr.finish = fakeFinishTime;
                    curr.finishMetrics = fakeFinishMetrics;
                    curr.incl = diffMetrics(curr.startMetrics, fakeFinishMetrics);
                    curr.excl = exclMetrics(curr.incl, curr.children);
                }
                curr = curr.parent;
            }
        }
        messages.receiveCallgraphMessage = receiveCallgraphMessage;
        function receiveSolverCallsMessage(msg) {
            var events = msg["events"];
            if (!currentState.zeroMetrics) {
                console.error("solver-calls expects profile data first");
                return;
            }
            var startTime = currentState.zeroMetrics.time;
            for (var _i = 0, events_1 = events; _i < events_1.length; _i++) {
                var e = events_1[_i];
                if (e.type == "start") {
                    var typ = e.part == "solver" ? SolverCallType.SOLVE : (e.part == "encode" ? SolverCallType.ENCODE : SolverCallType.FINITIZE);
                    currentState.solverCalls.push({
                        type: typ,
                        start: e.time - startTime,
                        finish: undefined,
                        sat: undefined
                    });
                }
                else if (e.type == "finish") {
                    if (currentState.solverCalls.length > 0) {
                        var curr = currentState.solverCalls[currentState.solverCalls.length - 1];
                        curr.finish = e.time - startTime;
                        if (curr.type == SolverCallType.SOLVE)
                            curr.sat = e.sat;
                    }
                }
            }
        }
        messages.receiveSolverCallsMessage = receiveSolverCallsMessage;
        function receiveUnusedTermsMessage(msg) {
            var data = msg["data"]; // list of (call-id, #unused) pairs
            for (var _i = 0, data_2 = data; _i < data_2.length; _i++) {
                var pair = data_2[_i];
                var id = pair[0].toString(), num = pair[1];
                if (currentState.idToNode.hasOwnProperty(id)) {
                    var node = currentState.idToNode[id];
                    node.excl["unused-terms"] = num;
                }
            }
        }
        messages.receiveUnusedTermsMessage = receiveUnusedTermsMessage;
    })(messages || (messages = {}));
    var stream;
    (function (stream) {
        var webSocket;
        function receiveStreamMessage(msg) {
            if (msg["event"] == "start") {
                currentState.streaming = true;
                webSocket = new WebSocket(msg["url"]);
                webSocket.onmessage = webSocketMessageCallback;
                webSocket.onerror = webSocketErrorCallback;
            }
            else if (msg["event"] == "finish") {
                currentState.streaming = false;
                if (webSocket)
                    webSocket.close();
            }
            else {
                console.log("unknown stream message:", msg);
            }
        }
        stream.receiveStreamMessage = receiveStreamMessage;
        function webSocketMessageCallback(evt) {
            var msgs = JSON.parse(evt.data); // will be a list of messages
            receiveData(msgs);
        }
        function webSocketErrorCallback(evt) {
            alert("Could not open the WebSocket connection for streaming. This might happen if the profiler is not currently running.");
        }
    })(stream || (stream = {}));
    // hand messages to their handler functions
    function receiveMessages(msgs) {
        for (var _i = 0, msgs_1 = msgs; _i < msgs_1.length; _i++) {
            var msg = msgs_1[_i];
            if (msg.type == "metadata") {
                messages.receiveMetadataMessage(msg);
            }
            else if (msg.type == "callgraph") {
                messages.receiveCallgraphMessage(msg);
            }
            else if (msg.type == "solver-calls") {
                messages.receiveSolverCallsMessage(msg);
            }
            else if (msg.type == "unused-terms") {
                messages.receiveUnusedTermsMessage(msg);
            }
            else if (msg.type === "stream") {
                stream.receiveStreamMessage(msg);
            }
            else {
                console.log("unknown message:", msg);
            }
        }
    }
    function receiveData(msgs) {
        if (ready) {
            receiveMessages(msgs);
            for (var _i = 0, updateCallbacks_1 = updateCallbacks; _i < updateCallbacks_1.length; _i++) {
                var cb = updateCallbacks_1[_i];
                cb(currentState);
            }
        }
        else {
            bufferedMessages.push.apply(bufferedMessages, msgs);
        }
    }
    data_1.receiveData = receiveData;
})(data || (data = {}));
//# sourceMappingURL=data.js.map