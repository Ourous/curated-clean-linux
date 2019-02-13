L.Window = L.Control.extend({
    initialize: function (baseLayers, overlays, options) {
        this._relatedMarkers = {};
    },
    options: {
        position: 'topleft'
    },
    setInitPos: function(pos) {
        this._initPos = pos;
    },
    setTitle: function(title) {
        if (this._titleEl) {
            this._titleEl.innerHTML = title;
        } else {
            this._initTitle = title;
        }
    },
    setContent: function(content) {
        if (this._contentNode) {
            this._contentNode.innerHTML = content;
        } else {
            this._initContent = content;
        }
    },
    setRelatedMarkers: function(markers) {
        // remove all markers
        for (markerId in this._relatedMarkers) {
            if (markerId in this._relatedMarkerConnectors) {
                this._relatedMarkerConnectors[markerId].polyline.remove();
            }
        };
        this._relatedMarkers = {};
        this._relatedMarkerConnectors = {};

        markers.forEach((marker) => this.addRelatedMarker(marker));
        // this actually creates the connectors
        this._map.eachLayer((l) => this._onLayerAdd({layer: l}));
    },
    addRelatedMarker: function(marker) {
        const markerId   = marker[0];
        const lineStyles = marker[1];
        var options = {};

        lineStyles.forEach((lineStyle) => {
            const lineStyleConstr    = lineStyle[0];
            const lineStyleConstrArg = lineStyle[1];

            switch (lineStyleConstr) {
                case "Style":
                    const lineStyleAttr    = lineStyleConstrArg[0];
                    const lineStyleAttrVal = lineStyleConstrArg[1];

                    switch (lineStyleAttr) {
                        case "LineStrokeColor":
                            options.color     = lineStyleAttrVal;
                            break;
                        case "LineStrokeWidth":
                            options.weight    = lineStyleAttrVal;
                            break;
                        case "LineOpacity":
                            options.opacity   = lineStyleAttrVal;
                            break;
                        case "LineDashArray":
                            options.dashArray = lineStyleAttrVal;
                            break;
                        default:
                            throw new Error("Unknown line style attribute: " + lineStyleAttr);
                    }
                    break;
                case "Class":
                    options.className = lineStyleConstrArg;
                    break;
                default:
                    throw new Error("Unknown line style constructor: " + lineStyleConstr);
            }
        });

        this._relatedMarkers[markerId] = options;
    },
    onAdd: function (map) {
        this._map = map;
        const container = this._container = L.DomUtil.create('div', 'itasks-leaflet-window');

        // add title bar
        const titleBar = L.DomUtil.create('div', 'titlebar', container);

        const closeButton = this._closeButton = L.DomUtil.create('a', 'close', titleBar);
        closeButton.href = '#close';
        closeButton.innerHTML = 'x';
        L.DomEvent.on(closeButton, 'mouseup', this._onCloseButtonClick, this);

        const titleSpan = L.DomUtil.create('span', '', titleBar);
        titleSpan.innerHTML = this._initTitle;
        delete this._initTitle;
        this._titleEl = titleSpan;

        // add content container
        this._contentNode = L.DomUtil.create('div', '', container);
        this._contentNode.innerHTML = this._initContent;
        delete this._initContent;

        // absolute -> otherwise windows influence each other if multiple are present
        container.style = "margin: 0px; position: absolute;";
        this._setPos(this._initPos);

        L.DomEvent.disableClickPropagation(container);
        L.DomEvent.disableScrollPropagation(container);
        L.DomEvent.on(container, 'contextmenu', L.DomEvent.stopPropagation);
        this.dragging = false;
        L.DomEvent.on(titleBar, 'mousedown', function(e) {
            // store current size of map container & title bar
            // required to prevent title var from being dragged out of view
            const mapContainerSize   = this._map.getSize();
            this._mapContainerWidth  = mapContainerSize.x;
            this._mapContainerHeight = mapContainerSize.y;
            this._titleBarWidth      = titleBar.offsetWidth;
            this._titleBarHeight     = titleBar.offsetHeight;

            // store delta between left upper corner of window and mouse position
            const containerRect = this._container.getBoundingClientRect();
            this.dragging = [e.clientX - containerRect.left, e.clientY - containerRect.top];
            L.DomUtil.disableTextSelection();
            this._container.style.opacity = 0.6;
            L.DomUtil.toFront(container);
        }, this);
        L.DomEvent.on(document, 'mouseup', this._mouseUp, this);
        L.DomEvent.on(document, 'mousemove', this._mouseMove, this);

        this._relatedMarkerConnectors = {};
        // handle related markers added in the future
        // markers already added are handled in 'addTo'
        map.on('layeradd', this._onLayerAdd, this);
        // handle related markers removed in the future
        map.on('layerremove', this._onLayerRemove, this);
        // update related marker connectors positions on zoom/move
        map.on('zoom', this._updateRelatedMarkerConnectorPositions, this);
        map.on('move', this._updateRelatedMarkerConnectorPositions, this);

        return container;
    },
    addTo: function(map) {
        L.Control.prototype.addTo.call(this, map);
        // handle related markers already added.
        // this is done after adding the window,
        // as we need the content's size
        // to position the connectors properly
        map.eachLayer((l) => this._onLayerAdd({layer: l}));
    },
    _onLayerAdd: function(e) {
        const marker = e.layer;
        if (marker.markerId in this._relatedMarkers) {
            this._addRelatedMarker(marker, this._relatedMarkers[marker.markerId]);
        }
    },
    _onLayerRemove: function(e) {
        const markerId = e.layer.markerId;
        if (markerId in this._relatedMarkers) {
            this._relatedMarkerConnectors[markerId].polyline.remove();
            delete this._relatedMarkerConnectors[markerId];
        }
    },
    _addRelatedMarker: function(marker, options) {
        const connector = {polyline: L.polyline([], options), position: marker.getLatLng()};
        this._relatedMarkerConnectors[marker.markerId] = connector;
        connector.polyline.addTo(this._map);
        this._updateRelatedMarkerConnectorPosition(connector);
    },
    _updateRelatedMarkerConnectorPositions: function() {
        Object.values(this._relatedMarkerConnectors).forEach(this._updateRelatedMarkerConnectorPosition, this);
    },
    _updateRelatedMarkerConnectorPosition: function(connector) {
        const windowCentrePos = { x: this._position.x + this._container.offsetWidth/2
                                , y: this._position.y + this._container.offsetHeight/2 };
        connector.polyline.setLatLngs([this._map.containerPointToLatLng(windowCentrePos), connector.position]);
    },
    _mouseUp: function(e) {
        this.dragging = false;
        L.DomUtil.enableTextSelection();
        this._container.style.opacity = 1.0;
    },
    _mouseMove: function(e) {
        const dragging = this.dragging;
        if (dragging) {
            const mapPos = this._map.mouseEventToContainerPoint(e);
            // delta (stored in 'dragging') to compensate for where inside title bar drag was started
            // restrict position such that title bar is never dragged outside of map container
            const x = Math.min(this._mapContainerWidth  - this._titleBarWidth,  Math.max(0, mapPos.x - dragging[0]));
            const y = Math.min(this._mapContainerHeight - this._titleBarHeight, Math.max(0, mapPos.y - dragging[1]));
            this._setPos({x: x, y: y});
            this._updateRelatedMarkerConnectorPositions();
        }
    },
    _onCloseButtonClick: function () {
        this._onWindowClose(); // injected by Clean code (sends remove event to server)
        this.remove();
    },
    _setPos: function(p) {
        this._position = p;
        this._container.style.transform = "translate(" + p.x + "px, " + p.y + "px)";
    },
    onRemove: function (map) {
        Object.values(this._relatedMarkerConnectors).forEach((m) => m.polyline.remove());
        L.DomEvent.off(document, 'mouseup', this._mouseUp, this);
        L.DomEvent.off(document, 'mousemove', this._mouseMove, this);
        map.off('layeradd', this._onLayerAdd, this);
        map.off('layerremove', this._onLayerRemove, this);
        map.off('zoom', this._updateRelatedMarkerConnectorPositions, this);
        map.off('move', this._updateRelatedMarkerConnectorPositions, this);
    }
});
L.window = function (options, source) {
    return new L.Window(options, source);
};
