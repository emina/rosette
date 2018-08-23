var d3_stackgraph;
(function (d3_stackgraph) {
    var PADDING_X_LEFT = 30;
    var PADDING_X_RIGHT = 20;
    var PADDING_Y_BOTTOM = 18;
    var X_AXIS_TICKS = 10;
    var MIN_WIDTH = 0.5; // px
    var TOOLTIP_KEY = "stackGraphTooltip";
    var StackGraphNode = /** @class */ (function () {
        function StackGraphNode(data) {
            this.start = 0;
            this.finish = 1;
            this.depth = 0; // root has depth 0
            this.data = data;
            this.children = [];
        }
        StackGraphNode.prototype.descendants = function () {
            var nodes = [this];
            var descs = [];
            while (nodes.length > 0) {
                var node = nodes.pop();
                descs.push(node);
                for (var _i = 0, _a = node.children; _i < _a.length; _i++) {
                    var c = _a[_i];
                    nodes.push(c);
                }
            }
            return descs;
        };
        return StackGraphNode;
    }());
    var unzoomData = {
        start: 0, finish: 0, parent: null, children: [], name: "<unzoom>", id: "unzoom"
    };
    var StackGraph = /** @class */ (function () {
        function StackGraph(s) {
            this._width = 960;
            this._cellHeight = 18;
            this._transitionDuration = 750;
            this._transitionEase = d3.easeCubic;
            this._data = null;
            this._highlights = [];
            this._color = function (d) { return "#00ff00"; };
            this._textColor = function (d) { return "#000000"; };
            this._discontinuities = [];
            this.zoomed = false;
            this.nextId = 0;
            this.idToNode = {};
            this.mouseoverCallback = null;
            this.mouseoutCallback = null;
            this.selector = s;
        }
        StackGraph.prototype.width = function (x) {
            if (typeof x === "undefined")
                return this._width;
            this._width = x;
            // this.render(true);
            return this;
        };
        StackGraph.prototype.height = function (x) {
            if (typeof x === "undefined")
                return this._height;
            this._height = x;
            // this.render(true);
            return this;
        };
        StackGraph.prototype.cellHeight = function (x) {
            if (typeof x === "undefined")
                return this._cellHeight;
            this._cellHeight = x;
            // this.render(true);
            return this;
        };
        StackGraph.prototype.clickHandler = function (x) {
            if (typeof x === "undefined")
                return this._clickHandler;
            this._clickHandler = x;
            return this;
        };
        StackGraph.prototype.hoverHandler = function (x) {
            if (typeof x === "undefined")
                return this._hoverHandler;
            this._hoverHandler = x;
            return this;
        };
        StackGraph.prototype.data = function (x) {
            if (typeof x === "undefined")
                return this._data;
            this._data = x;
            if (!this.zoomed) {
                this.root = this.partition(x);
            }
            return this;
        };
        StackGraph.prototype.highlights = function (x) {
            if (typeof x === "undefined")
                return this._highlights;
            this._highlights = x;
            return this;
        };
        StackGraph.prototype.color = function (x) {
            if (typeof x === "undefined")
                return this._color;
            this._color = x;
            return this;
        };
        StackGraph.prototype.textColor = function (x) {
            if (typeof x === "undefined")
                return this._textColor;
            this._textColor = x;
            return this;
        };
        StackGraph.prototype.discontinuities = function (x) {
            if (typeof x === "undefined")
                return this._discontinuities;
            this._discontinuities = x;
            return this;
        };
        StackGraph.prototype.clickCallback = function (d) {
            if (!this._clickHandler)
                return;
            if (d.data === unzoomData) {
                this._clickHandler(this._data);
            }
            else if (d.data !== this._data) {
                this._clickHandler(d.data);
            }
        };
        StackGraph.prototype.zoom = function (d) {
            if (d === this._data) {
                this.zoomed = false;
                this.root = this.partition(this._data);
                this.render(true);
            }
            else {
                this.zoomed = true;
                this.root = this.partition(d);
                this.render(true);
            }
        };
        StackGraph.prototype.highlightData = function (x) {
            var _this = this;
            // map the data to their corresponding nodes; remove those that don't exist
            var nodes = x.map(function (d) { return _this.idToNode[d.id]; }).filter(function (d) { return d; });
            var g = this.svg.selectAll("g.stack-graph-node").data(nodes, function (d) { return d.data.id; });
            // highlight selected nodes
            g.select("rect").classed("stack-graph-node-highlight", true);
            // unhighlight all other nodes
            g.exit().select("rect").classed("stack-graph-node-highlight", false);
        };
        StackGraph.prototype.partition = function (rootData) {
            this.idToNode = {};
            // first pass: build and layout the hierarchy
            var root = new StackGraphNode(rootData);
            root.start = rootData.start;
            root.finish = rootData.finish;
            this.idToNode[rootData.id] = root;
            var nodes = [root];
            var maxHeight = 0;
            while (nodes.length > 0) {
                var node = nodes.pop();
                if (!node.data.id) {
                    node.data.id = this.nextId.toString();
                    this.nextId += 1;
                }
                for (var _i = 0, _a = node.data.children; _i < _a.length; _i++) {
                    var c = _a[_i];
                    var cn = new StackGraphNode(c);
                    cn.parent = node;
                    cn.depth = node.depth + 1;
                    cn.start = c.start;
                    cn.finish = c.finish;
                    if (cn.depth > maxHeight)
                        maxHeight = cn.depth;
                    this.idToNode[cn.data.id] = cn;
                    node.children.push(cn);
                    nodes.push(cn);
                }
            }
            // finally set height if it's not default
            var computedHeight = Math.max(maxHeight + 1, 5) * this._cellHeight + PADDING_Y_BOTTOM;
            if (!this._height || (rootData === this._data && this._height > computedHeight)) {
                this._height = computedHeight;
            }
            return root;
        };
        StackGraph.prototype.setupSvg = function () {
            this.svg = d3.select(this.selector).append('svg').attr("class", "stack-graph");
            this.svg.append("g").attr("class", "stack-graph-highlights");
            this.svg.append("g").attr("class", "stack-graph-body");
            var labels = this.svg.append("g").attr("class", "stack-graph-labels");
            labels.append("rect").attr("class", "stack-graph-label-bg").attr("fill", "#ffffff");
            labels.append("text").attr("text-anchor", "middle")
                .attr("transform", "rotate(270)")
                .text("Call Stack");
            var axes = this.svg.append("g").attr("class", "stack-graph-axis");
            axes.append("rect").attr("class", "stack-graph-axis-bg").attr("fill", "#ffffff");
            this.svg.append("g").attr("class", "stack-graph-breaks");
            var this_ = this;
            var hasClass = function (elt, id) {
                if (elt.classList)
                    return elt.classList.contains(id);
                else if (elt.className.baseVal)
                    return elt.className.baseVal.indexOf(id) > -1;
                else
                    return elt.className.indexOf(id) > -1;
            };
            var mouseOpCallback = function (mouseover) {
                return function (d, i, x) {
                    var elt = this;
                    if (this_._hoverHandler) {
                        if (hasClass(elt, "stack-graph-highlight") || hasClass(elt, "stack-graph-break")) {
                            if (mouseover)
                                tooltip.show(d.summary, elt, "top");
                            else
                                tooltip.hide();
                        }
                        else if (hasClass(elt, "stack-graph-node")) {
                            this_._hoverHandler(d.data, mouseover);
                        }
                    }
                };
            };
            this.mouseoverCallback = mouseOpCallback(true);
            this.mouseoutCallback = mouseOpCallback(false);
        };
        StackGraph.prototype.render = function (anim) {
            var _this = this;
            if (anim === void 0) { anim = false; }
            // set up SVG
            if (!this.svg) {
                this.setupSvg();
            }
            this.svg.attr('width', this._width)
                .attr('height', this._height);
            var data = this.root ? this.root.descendants() : [];
            if (this.zoomed) {
                var curr = this.root.data, route = [];
                while (curr = curr.parent)
                    route.push(curr.name);
                unzoomData.name = "<unzoom> " + route.join(" ← ");
                var unzoomNode = new StackGraphNode(unzoomData);
                unzoomNode.start = this.root.start;
                unzoomNode.finish = this.root.finish;
                unzoomNode.depth = -1;
                data.splice(0, 0, unzoomNode);
            }
            ///////////////////////////////////////////////////////////////////
            // update y label position
            var gLabel = this.svg.select("g.stack-graph-labels");
            gLabel.select("rect").attr("x", 0)
                .attr("y", 0)
                .attr("width", PADDING_X_LEFT)
                .attr("height", this._height);
            gLabel.select("text").attr("dy", PADDING_X_LEFT / 2)
                .attr("dx", -this._height / 2);
            ///////////////////////////////////////////////////////////////////
            // create axes
            var xBase = d3.scaleLinear().range([PADDING_X_LEFT, this._width - PADDING_X_RIGHT]);
            if (this.root) {
                xBase.domain([this.root.start, this.root.finish]);
            }
            var _a = xBase.domain(), xMin = _a[0], xMax = _a[1];
            var xBaseTo01 = d3.scaleLinear().domain([xMin, xMax]);
            var x = fc.scaleDiscontinuous(xBase);
            var dcRange = fc.discontinuityRange.apply(fc, this._discontinuities);
            x.discontinuityProvider(dcRange);
            var xTo01 = fc.scaleDiscontinuous(xBaseTo01);
            xTo01.discontinuityProvider(dcRange);
            var ticks = d3.ticks(0, 1, X_AXIS_TICKS);
            var tickValues = ticks.map(xTo01.invert);
            var y = d3.scaleLinear().range([0, this._cellHeight]);
            var xAxis = d3.axisBottom(x).tickValues(tickValues);
            xAxis.tickFormat(function (x) { return (x / 1000).toFixed(3) + "s"; });
            var xAxisPos = this._height - PADDING_Y_BOTTOM;
            this.svg.select("g.stack-graph-axis")
                .attr("transform", "translate(0," + xAxisPos + ")")
                .call(xAxis);
            this.svg.select("rect.stack-graph-axis-bg")
                .attr("x", 0).attr("y", 0).attr("width", this._width).attr("height", PADDING_Y_BOTTOM);
            var width = function (d) { return x(d.finish) - x(d.start); };
            ///////////////////////////////////////////////////////////////////
            // render highlights
            var gHigh = this.svg.select("g.stack-graph-highlights");
            var hRect = gHigh.selectAll("rect").data(this._highlights.filter(function (d) { return width(d) > 0; }));
            var hEnter = hRect.enter().append("svg:rect").attr("class", "stack-graph-highlight");
            hEnter.on("mouseover", this.mouseoverCallback)
                .on("mouseout", this.mouseoutCallback);
            hRect.merge(hEnter)
                .attr("height", this._height)
                .attr("fill", function (d) { return d.color; })
                .attr("title", function (d) { return d.summary; });
            // animate the highlights if requested
            var hPos = function (grp) { return grp.attr("width", width)
                .attr("transform", function (d) { return "translate(" + x(d.start) + ",0)"; }); };
            if (anim) {
                hPos(hRect.transition()
                    .duration(this._transitionDuration).ease(this._transitionEase));
            }
            else {
                hPos(hRect);
            }
            hPos(hEnter);
            hRect.exit().remove();
            ///////////////////////////////////////////////////////////////////
            // render breaks
            var gBreak = this.svg.select("g.stack-graph-breaks");
            var bLine = gBreak.selectAll("line").data(this._highlights.filter(function (d) { return width(d) == 0; }));
            var bEnter = bLine.enter().append("svg:line").attr("class", "stack-graph-break");
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
            var bPos = function (grp) { return grp.attr("transform", function (d) { return "translate(" + x(d.start) + ",0)"; }); };
            if (anim) {
                bPos(bLine.transition()
                    .duration(this._transitionDuration).ease(this._transitionEase));
            }
            else {
                bPos(bLine);
            }
            bPos(bEnter);
            bLine.exit().remove();
            ///////////////////////////////////////////////////////////////////
            // render call stacks
            var dx = function (d) { return x(d.start); };
            var dy = function (d) { return _this._height - y(d.depth + (_this.zoomed ? 1 : 0)) - _this._cellHeight - PADDING_Y_BOTTOM; };
            var pos = function (d) { return "translate(" + dx(d) + "," + dy(d) + ")"; };
            var rectClass = function (d) { return d.data === unzoomData ? "stack-graph-rect stack-graph-unzoom" : "stack-graph-rect"; };
            var filteredData = data.filter(function (d) { return width(d) >= MIN_WIDTH; });
            var g = this.svg.select("g.stack-graph-body")
                .selectAll("g.stack-graph-node").data(filteredData, function (d) { return d.data.id; });
            var enter = g.enter().append("svg:g");
            enter.append("svg:rect");
            enter.append("foreignObject")
                .append("xhtml:div");
            enter.on("click", function (d) { return _this.clickCallback(d); })
                .on("mouseover", this.mouseoverCallback)
                .on("mouseout", this.mouseoutCallback);
            g.merge(enter)
                .attr("height", this._cellHeight)
                .attr("name", function (d) { return d.data.name; })
                .attr("class", "stack-graph-node")
                .select("rect")
                .attr("height", this._cellHeight)
                .attr("class", rectClass)
                .attr("fill", function (d) { return _this._color(d.data); });
            g.merge(enter)
                .select("foreignObject")
                .attr("width", width)
                .attr("height", this._cellHeight)
                .select("div")
                .attr("class", "stack-graph-label")
                .style("display", function (d) { return width(d) < 35 ? "none" : "block"; })
                .style("color", function (d) { return _this._textColor(d.data); })
                .text(function (d) { return d.data.name; });
            // animate preexisting nodes into new positions
            var gPos = function (grp) { return grp.attr("transform", pos)
                .attr("width", width)
                .select("rect")
                .attr("width", width); };
            if (anim) {
                gPos(g.transition()
                    .duration(this._transitionDuration).ease(this._transitionEase));
            }
            else {
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
        };
        return StackGraph;
    }());
    d3_stackgraph.StackGraph = StackGraph;
    function stackGraph(selector) {
        return new StackGraph(selector);
    }
    d3_stackgraph.stackGraph = stackGraph;
})(d3_stackgraph || (d3_stackgraph = {}));
//# sourceMappingURL=d3-stack-graph.js.map