declare var d3;
declare var fc;

namespace d3_stackgraph {
    type ClickHandler = (d: StackGraphData) => any;
    type HoverHandler = (d: StackGraphData, enter: boolean) => any;
    type ColorHandler = (d: StackGraphData) => string;
    type D3Selection = any;

    const PADDING_X_LEFT = 30;
    const PADDING_X_RIGHT = 20;
    const PADDING_Y_BOTTOM = 18;
    const X_AXIS_TICKS = 10;
    const MIN_WIDTH = 0.5; // px

    const TOOLTIP_KEY = "stackGraphTooltip";

    class StackGraphNode {
        data: StackGraphData;
        start: number = 0;
        finish: number = 1;
        parent: StackGraphNode;
        children: StackGraphNode[];
        depth: number = 0;  // root has depth 0

        constructor(data: StackGraphData) {
            this.data = data;
            this.children = [];
        }

        descendants(): StackGraphNode[] {
            let nodes: StackGraphNode[] = [this];
            let descs = [];
            while (nodes.length > 0) {
                let node = nodes.pop();
                descs.push(node);
                for (let c of node.children) nodes.push(c);
            }
            return descs;
        }
    }

    export interface StackGraphData {
        start: number;
        finish: number;
        parent: StackGraphData;
        children: StackGraphData[];
        name: string;
        id: string;
    }

    let unzoomData: StackGraphData = {
        start: 0, finish: 0, parent: null, children: [], name: "<unzoom>", id: "unzoom"
    };

    export interface StackGraphHighlight {
        start: number;
        finish: number;
        color: string;
        summary: string;
    }

    export class StackGraph {
        private _width: number = 960;
        private _height: number;
        private _cellHeight: number = 18;
        private _selection: D3Selection;
        private _transitionDuration: number = 750;
        private _transitionEase: any = d3.easeCubic;
        private _clickHandler: ClickHandler;
        private _hoverHandler: HoverHandler;
        private _data: StackGraphData = null;
        private _highlights: StackGraphHighlight[] = [];
        private _color: ColorHandler = (d) => "#00ff00";
        private _textColor: ColorHandler = (d) => "#000000";
        private _discontinuities: number[][] = [];

        private root: StackGraphNode;
        private svg: any;
        private selector: string;
        private zoomed: boolean = false;
        private nextId: number = 0;
        private idToNode: Map<StackGraphNode> = {};
        private mouseoverCallback: (d: Object, i: number, x: Object[]) => void = null;
        private mouseoutCallback: (d: Object, i: number, x: Object[]) => void = null;

        width(): number
        width(x: number): StackGraph
        width(x?: number): number | StackGraph {
            if (typeof x === "undefined") return this._width;
            this._width = x;
            // this.render(true);
            return this;
        }
        height(): number
        height(x: number): StackGraph
        height(x?: number): number | StackGraph {
            if (typeof x === "undefined") return this._height;
            this._height = x;
            // this.render(true);
            return this;
        }
        cellHeight(): number
        cellHeight(x: number): StackGraph
        cellHeight(x?: number): number | StackGraph {
            if (typeof x === "undefined") return this._cellHeight;
            this._cellHeight = x;
            // this.render(true);
            return this;
        }
        clickHandler(): ClickHandler
        clickHandler(x: ClickHandler): StackGraph
        clickHandler(x?: ClickHandler): ClickHandler | StackGraph {
            if (typeof x === "undefined") return this._clickHandler;
            this._clickHandler = x;
            return this;
        }
        hoverHandler(): HoverHandler
        hoverHandler(x: HoverHandler): StackGraph
        hoverHandler(x?: HoverHandler): HoverHandler | StackGraph {
            if (typeof x === "undefined") return this._hoverHandler;
            this._hoverHandler = x;
            return this;
        }
        data(): StackGraphData
        data(x: StackGraphData): StackGraph
        data(x?: StackGraphData): StackGraphData | StackGraph {
            if (typeof x === "undefined") return this._data;
            this._data = x;
            if (!this.zoomed) {
                this.root = this.partition(x);
            }
            return this;
        }
        highlights(): StackGraphHighlight[]
        highlights(x: StackGraphHighlight[]): StackGraph
        highlights(x?: StackGraphHighlight[]): StackGraphHighlight[] | StackGraph {
            if (typeof x === "undefined") return this._highlights;
            this._highlights = x;
            return this;
        }
        color(): ColorHandler
        color(x: ColorHandler): StackGraph
        color(x?: ColorHandler): ColorHandler | StackGraph {
            if (typeof x === "undefined") return this._color;
            this._color = x;
            return this;
        }
        textColor(): ColorHandler
        textColor(x: ColorHandler): StackGraph
        textColor(x?: ColorHandler): ColorHandler | StackGraph {
            if (typeof x === "undefined") return this._textColor;
            this._textColor = x;
            return this;
        }
        discontinuities(): number[][]
        discontinuities(x: number[][]): StackGraph
        discontinuities(x?: number[][]): number[][] | StackGraph {
            if (typeof x === "undefined") return this._discontinuities;
            this._discontinuities = x;
            return this;
        }

        clickCallback(d: StackGraphNode) {
            if (!this._clickHandler) return;

            if (d.data === unzoomData) {
                this._clickHandler(this._data);
            } else if (d.data !== this._data) {
                this._clickHandler(d.data);
            }
        }

        zoom(d: StackGraphData) {
            if (d === this._data) {
                this.zoomed = false;
                this.root = this.partition(this._data);
                this.render(true);
            } else {
                this.zoomed = true;
                this.root = this.partition(d);
                this.render(true);
            }
        }

        highlightData(x: StackGraphData[]) {
            // map the data to their corresponding nodes; remove those that don't exist
            let nodes = x.map(d => this.idToNode[d.id]).filter(d => d);

            let g = this.svg.selectAll("g.stack-graph-node").data(nodes, (d) => d.data.id);
            // highlight selected nodes
            g.select("rect").classed("stack-graph-node-highlight", true);
            // unhighlight all other nodes
            g.exit().select("rect").classed("stack-graph-node-highlight", false);
        }

        partition(rootData: StackGraphData): StackGraphNode {
            this.idToNode = {};
            // first pass: build and layout the hierarchy
            let root = new StackGraphNode(rootData);
            root.start = rootData.start;
            root.finish = rootData.finish;
            this.idToNode[rootData.id] = root;
            let nodes = [root];
            var maxHeight = 0;
            while (nodes.length > 0) {
                let node = nodes.pop();
                if (!node.data.id) {
                    node.data.id = this.nextId.toString();
                    this.nextId += 1;
                }
                for (let c of node.data.children) {
                    let cn = new StackGraphNode(c);
                    cn.parent = node;
                    cn.depth = node.depth + 1;
                    cn.start = c.start;
                    cn.finish = c.finish;
                    if (cn.depth > maxHeight) maxHeight = cn.depth;
                    this.idToNode[cn.data.id] = cn;
                    node.children.push(cn);
                    nodes.push(cn);
                }
            }
            // finally set height if it's not default
            let computedHeight = Math.max(maxHeight + 1, 5) * this._cellHeight + PADDING_Y_BOTTOM;
            if (!this._height || (rootData === this._data && this._height > computedHeight)) {
                this._height = computedHeight;
            }
            return root;
        }

        setupSvg() {
            this.svg = d3.select(this.selector).append('svg').attr("class", "stack-graph");
            this.svg.append("g").attr("class", "stack-graph-highlights");
            this.svg.append("g").attr("class", "stack-graph-body");
            let labels = this.svg.append("g").attr("class", "stack-graph-labels");
            labels.append("rect").attr("class", "stack-graph-label-bg").attr("fill", "#ffffff");
            labels.append("text").attr("text-anchor", "middle")
                                 .attr("transform", "rotate(270)")
                                 .text("Call Stack");
            let axes = this.svg.append("g").attr("class", "stack-graph-axis");
            axes.append("rect").attr("class", "stack-graph-axis-bg").attr("fill", "#ffffff");
            this.svg.append("g").attr("class", "stack-graph-breaks");

            let this_ = this;
            let hasClass = (elt, id) => {
                if (elt.classList) return elt.classList.contains(id);
                else if (elt.className.baseVal) return elt.className.baseVal.indexOf(id) > -1;
                else return elt.className.indexOf(id) > -1;
            }
            let mouseOpCallback = function(mouseover) {
                return function(d, i, x) {
                    let elt = <HTMLElement> this;
                    if (this_._hoverHandler) {
                        if (hasClass(elt, "stack-graph-highlight") || hasClass(elt, "stack-graph-break")) {
                            if (mouseover) tooltip.show((<StackGraphHighlight>d).summary, elt, "top");
                            else tooltip.hide();
                        } else if (hasClass(elt, "stack-graph-node")) {
                            this_._hoverHandler((<StackGraphNode>d).data, mouseover);
                        }
                    }
                };
            };
            this.mouseoverCallback = mouseOpCallback(true);
            this.mouseoutCallback = mouseOpCallback(false);
        }

        render(anim: boolean = false) {
            // set up SVG
            if (!this.svg) {
                this.setupSvg();
            }
            this.svg.attr('width', this._width)
                    .attr('height', this._height);

            let data = this.root ? this.root.descendants() : [];
            if (this.zoomed) {
                let curr = this.root.data, route = [];
                while (curr = curr.parent) route.push(curr.name);
                unzoomData.name = "<unzoom> " + route.join(" ← "); 
                let unzoomNode = new StackGraphNode(unzoomData);
                unzoomNode.start = this.root.start;
                unzoomNode.finish = this.root.finish;
                unzoomNode.depth = -1;
                data.splice(0, 0, unzoomNode);
            }


            ///////////////////////////////////////////////////////////////////
            // update y label position

            let gLabel = this.svg.select("g.stack-graph-labels");
            gLabel.select("rect").attr("x", 0)
                                 .attr("y", 0)
                                 .attr("width", PADDING_X_LEFT)
                                 .attr("height", this._height);
            gLabel.select("text").attr("dy", PADDING_X_LEFT/2)
                                 .attr("dx", -this._height/2);


            ///////////////////////////////////////////////////////////////////
            // create axes

            let xBase = d3.scaleLinear().range([PADDING_X_LEFT, this._width - PADDING_X_RIGHT]);
            if (this.root) {
                xBase.domain([this.root.start, this.root.finish]);
            }
            let [xMin, xMax] = xBase.domain();
            let xBaseTo01 = d3.scaleLinear().domain([xMin, xMax]);
            let x = fc.scaleDiscontinuous(xBase);
            let dcRange = fc.discontinuityRange(...this._discontinuities);
            x.discontinuityProvider(dcRange);
            let xTo01 = fc.scaleDiscontinuous(xBaseTo01);
            xTo01.discontinuityProvider(dcRange);

            let ticks = d3.ticks(0, 1, X_AXIS_TICKS);
            let tickValues = ticks.map(xTo01.invert);

            let y = d3.scaleLinear().range([0, this._cellHeight]);

            let xAxis = d3.axisBottom(x).tickValues(tickValues);
            xAxis.tickFormat((x) => (x/1000).toFixed(3) + "s");

            let xAxisPos = this._height - PADDING_Y_BOTTOM;
            this.svg.select("g.stack-graph-axis")
                .attr("transform", `translate(0,${xAxisPos})`)
                .call(xAxis);
            this.svg.select("rect.stack-graph-axis-bg")
                .attr("x", 0).attr("y", 0).attr("width", this._width).attr("height", PADDING_Y_BOTTOM);

            let width = (d: StackGraphNode | StackGraphHighlight) => x(d.finish) - x(d.start);

            ///////////////////////////////////////////////////////////////////
            // render highlights

            let gHigh = this.svg.select("g.stack-graph-highlights");
            let hRect = gHigh.selectAll("rect").data(this._highlights.filter((d) => width(d) > 0));
            let hEnter = hRect.enter().append("svg:rect").attr("class", "stack-graph-highlight");
            hEnter.on("mouseover", this.mouseoverCallback)
                  .on("mouseout", this.mouseoutCallback);
            hRect.merge(hEnter)
                .attr("height", this._height)
                .attr("fill", (d: StackGraphHighlight) => d.color)
                .attr("title", (d: StackGraphHighlight) => d.summary);

            // animate the highlights if requested
            let hPos = (grp) => grp.attr("width", width)
                                   .attr("transform", (d) => "translate(" + x(d.start) + ",0)");
            if (anim) {
                hPos(hRect.transition()
                       .duration(this._transitionDuration).ease(this._transitionEase));
            } else {
                hPos(hRect);
            }
            hPos(hEnter);
            hRect.exit().remove();

            ///////////////////////////////////////////////////////////////////
            // render breaks

            let gBreak = this.svg.select("g.stack-graph-breaks");
            let bLine = gBreak.selectAll("line").data(this._highlights.filter((d) => width(d) == 0));
            let bEnter = bLine.enter().append("svg:line").attr("class", "stack-graph-break");
            bEnter.on("mouseover", this.mouseoverCallback)
                  .on("mouseout", this.mouseoutCallback);
            bLine.merge(bEnter)
                .attr("y1", this._height - PADDING_Y_BOTTOM + xAxis.tickSizeInner())
                .attr("y2", 0)
                .attr("x1", 0)
                .attr("x2", 0)
                .attr("stroke-width", 1)
                .attr("stroke", "#444444")
                .style("stroke-dasharray", "5 5");
            
            // animate the breaks if requested
            let bPos = (grp) => grp.attr("transform", (d) => "translate(" + x(d.start) + ",0)");
            if (anim) {
                bPos(bLine.transition()
                       .duration(this._transitionDuration).ease(this._transitionEase));
            } else {
                bPos(bLine);
            }
            bPos(bEnter);
            bLine.exit().remove();


            ///////////////////////////////////////////////////////////////////
            // render call stacks

            let dx = (d: StackGraphNode) => x(d.start);
            let dy = (d: StackGraphNode) => this._height - y(d.depth + (this.zoomed ? 1 : 0)) - this._cellHeight - PADDING_Y_BOTTOM;
            let pos = (d: StackGraphNode) => `translate(${dx(d)},${dy(d)})`;
            let rectClass = (d: StackGraphNode) => d.data === unzoomData ? "stack-graph-rect stack-graph-unzoom" : "stack-graph-rect";

            let filteredData = data.filter((d) => width(d) >= MIN_WIDTH);
            let g = this.svg.select("g.stack-graph-body")
                            .selectAll("g.stack-graph-node").data(filteredData, (d) => d.data.id);

            let enter = g.enter().append("svg:g");
            enter.append("svg:rect");
            enter.append("foreignObject")
                 .append("xhtml:div");
            enter.on("click", (d: StackGraphNode) => this.clickCallback(d))
                 .on("mouseover", this.mouseoverCallback)
                 .on("mouseout", this.mouseoutCallback);

            g.merge(enter)
                .attr("height", this._cellHeight)
                .attr("name", (d: StackGraphNode) => d.data.name)
                .attr("class", "stack-graph-node")
              .select("rect")
                .attr("height", this._cellHeight)
                .attr("class", rectClass)
                .attr("fill", (d: StackGraphNode) => this._color(d.data));
            
            g.merge(enter)
              .select("foreignObject")
                .attr("width", width)
                .attr("height", this._cellHeight)
              .select("div")
                .attr("class", "stack-graph-label")
                .style("display", (d: StackGraphNode) => width(d) < 35 ? "none" : "block")
                .style("color", (d: StackGraphNode) => this._textColor(d.data))
                .text((d: StackGraphNode) => d.data.name);
            
            // animate preexisting nodes into new positions
            let gPos = (grp) => grp.attr("transform", pos)
                                   .attr("width", width)
                                  .select("rect")
                                   .attr("width", width);
            if (anim) {
                gPos(g.transition()
                      .duration(this._transitionDuration).ease(this._transitionEase));
            } else {
                gPos(g);
            }

            // don't animate new nodes into position...
            gPos(enter);
            // but maybe fade them in if this isn't the first render (i.e., if unzooming)
            if (anim) {
                enter.style("opacity", 0.0)
                  .transition()
                  .duration(this._transitionDuration).ease(this._transitionEase)
                    .style("opacity", 1.0);
            }

            g.exit().remove();
        }

        constructor(s: string) {
            this.selector = s;
        }

    }

    export function stackGraph(selector: string): StackGraph {
        return new StackGraph(selector);
    }
}