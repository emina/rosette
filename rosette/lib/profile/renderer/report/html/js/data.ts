namespace data {
    export interface CallNode {
        id: string;
        name: string;
        callsite: string;
        source: string;
        start: number;  // time
        finish: number;  // time
        startMetrics: Map<number>;
        finishMetrics: Map<number>;
        isFinished: boolean;  // have we seen an exit event for this node?
        inputs: Map<any>;
        outputs: Map<any>;
        children: CallNode[];
        parent: CallNode;
        incl: Map<number>;
        excl: Map<number>;
        score: number;
    }

    export interface ProfileState {
        metadata: Object;
        root: CallNode;
        current: CallNode;
        idToNode: Map<CallNode>;
        zeroMetrics: Map<number>;
        solverCalls: SolverCall[];
        streaming: boolean;
    }

    export interface SolverCall {
        type: SolverCallType,
        start: number;
        finish: number;
        sat: boolean;
    }

    export enum SolverCallType {
        SOLVE,
        ENCODE,
        FINITIZE
    };

    // events are also a type of message, since they have a type field
    interface Message {
        type: string
    }

    // buffer messages until document ready
    var bufferedMessages: Message[] = [];
    var ready = false;
    var currentState: ProfileState = {
        metadata: null,
        root: null,
        current: null,
        idToNode: {},
        zeroMetrics: null,
        solverCalls: [],
        streaming: false
    };

    // callbacks to invoke when new state data arrives

    type StateUpdateCallback = (msg: ProfileState) => void;
    var updateCallbacks: StateUpdateCallback[] = [];

    export function registerUpdateCallback(cb: StateUpdateCallback): void {
        updateCallbacks.push(cb);
    }
    export function unregisterUpdateCallback(cb: StateUpdateCallback): void {
        let i = updateCallbacks.indexOf(cb);
        if (i > -1)
            updateCallbacks.splice(i, 1);
    }

    export function readyForData() {
        if (!ready) {
            ready = true;
            receiveData(bufferedMessages);
            bufferedMessages = [];
        }
    }

    function diffMetrics(p1: Map<number>, p2: Map<number>): Map<number> {
        let ret: Map<number> = {};
        for (let k in p1) {
            if (p2.hasOwnProperty(k)) {
                ret[k] = p2[k] - p1[k];
            }
        }
        return ret;
    }

    function exclMetrics(incl: Map<number>, children: CallNode[]): Map<number> {
        let ret = {};
        for (let k in incl) {
            ret[k] = incl[k];
        }
        for (let c of children) {
            for (let k in incl) {
                ret[k] -= c.incl[k] || 0;
            }
        }
        return ret;
    }

    namespace messages {
        export function receiveMetadataMessage(msg: Message): void {
            delete msg["type"];
            currentState.metadata = msg;
        }

        // update the ProfileState using data from the profile message
        export function receiveCallgraphMessage(msg: Message): void {
            let evts = msg["events"];
            if (evts.length == 0) return;

            if (!currentState.zeroMetrics) {
                currentState.zeroMetrics = evts[0].metrics;
            }

            for (let e of evts) {
                if (e.type == "ENTER") {
                    if (!currentState.current && currentState.root) {
                        console.error("multiple root procedure calls");
                        return;
                    }
                    // e has fields:
                    //   id, function, location, inputs, metrics
                    let dm = diffMetrics(currentState.zeroMetrics, e.metrics);
                    let node: CallNode = {
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
                    if (!currentState.root)  // might be the first call
                        currentState.root = node;
                    currentState.current = node;
                    currentState.idToNode[e.id] = node;
                } else if (e.type == "EXIT") {
                    if (!currentState.current) {
                        console.error("unbalanced EXIT event");
                    }
                    // e has fields:
                    //   outputs, metrics
                    let dm = diffMetrics(currentState.zeroMetrics, e.metrics);
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
            let fakeFinishMetrics = diffMetrics(currentState.zeroMetrics, evts[evts.length-1].metrics);
            let fakeFinishTime = fakeFinishMetrics.time;
            let curr = currentState.current;
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

        export function receiveSolverCallsMessage(msg: Message): void {
            let events = msg["events"];
            if (!currentState.zeroMetrics) {
                console.error("solver-calls expects profile data first");
                return;
            }
            let startTime = currentState.zeroMetrics.time;
            for (let e of events) {
                if (e.type == "start") {
                    let typ = e.part == "solver" ? SolverCallType.SOLVE : (e.part == "encode" ? SolverCallType.ENCODE : SolverCallType.FINITIZE);
                    currentState.solverCalls.push({
                        type: typ,
                        start: e.time - startTime,
                        finish: undefined,
                        sat: undefined
                    });
                } else if (e.type == "finish") {
                    if (currentState.solverCalls.length > 0) {
                        let curr = currentState.solverCalls[currentState.solverCalls.length - 1];
                        curr.finish = e.time - startTime;
                        if (curr.type == SolverCallType.SOLVE)
                            curr.sat = e.sat;
                    }
                }
            }
        }

        export function receiveUnusedTermsMessage(msg: Message): void {
            let data: number[][] = msg["data"];  // list of (call-id, #unused) pairs
            for (let pair of data) {
                let id = pair[0].toString(), num = pair[1];
                if (currentState.idToNode.hasOwnProperty(id)) {
                    let node = currentState.idToNode[id];
                    node.excl["unused-terms"] = num;
                }
            }
        }
    }

    namespace stream {
        var webSocket: WebSocket;

        export function receiveStreamMessage(msg: Message): void {
            if (msg["event"] == "start") {
                currentState.streaming = true;
                webSocket = new WebSocket(msg["url"]);
                webSocket.onmessage = webSocketMessageCallback;
                webSocket.onerror = webSocketErrorCallback;
            } else if (msg["event"] == "finish") {
                currentState.streaming = false;
                if (webSocket) webSocket.close();
            } else {
                console.log("unknown stream message:", msg);
            }
        }

        function webSocketMessageCallback(evt: MessageEvent): void {
            let msgs = JSON.parse(evt.data); // will be a list of messages
            receiveData(msgs);
        }

        function webSocketErrorCallback(evt: Event) {
            alert("Could not open the WebSocket connection for streaming. This might happen if the profiler is not currently running.");
        }
    }

    // hand messages to their handler functions
    function receiveMessages(msgs: Message[]): void {
        for (let msg of msgs) {
            if (msg.type == "metadata") {
                messages.receiveMetadataMessage(msg);
            } else if (msg.type == "callgraph") {
                messages.receiveCallgraphMessage(msg);
            } else if (msg.type == "solver-calls") {
                messages.receiveSolverCallsMessage(msg);
            } else if (msg.type == "unused-terms") {
                messages.receiveUnusedTermsMessage(msg);
            } else if (msg.type === "stream") {
                stream.receiveStreamMessage(msg);
            } else {
                console.log("unknown message:", msg);
            }
        }
    }

    export function receiveData(msgs: Message[]): void {
        if (ready) {
            receiveMessages(msgs);
            for (let cb of updateCallbacks) {
                cb(currentState);
            }
        } else {
            bufferedMessages.push(...msgs);
        }
    }
}
