L.Window = L.Control.extend({
    options: {
        position: 'topleft'
    },
    setInitPos: function(pos) {
        this._initPos = pos;
    },
    setTitle: function(title) {
        this._title = title;
    },
    setContent: function(content) {
        this._content = content;
    },
    addRelatedMarker: function(markerId, options) {
        if (!this._relatedMarkers) this._relatedMarkers = {};

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
        titleSpan.innerHTML = this._title;

        // add content container
        this._contentNode = L.DomUtil.create('div', '', container);
        this._contentNode.innerHTML = this._content;

        // absolute -> otherwise windows influence each other if multiple are present
        container.style = "margin: 0px; position: absolute;";
        this._setPos(this._initPos);

        L.DomEvent.disableClickPropagation(container);
        L.DomEvent.disableScrollPropagation(container);
        L.DomEvent.on(container, 'contextmenu', L.DomEvent.stopPropagation);
        this.dragging = false;
        L.DomEvent.on(titleBar, 'mousedown', function(e) {
                                                 // store delta between left upper corner of window and mouse position
                                                 const containerRect = this._container.getBoundingClientRect();
                                                 this.dragging = [e.clientX - containerRect.left, e.clientY - containerRect.top];
                                                 L.DomUtil.disableTextSelection();
                                                 this._container.style.opacity = 0.6;
                                                 L.DomUtil.toFront(container);
                                             },
                      this);
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
        if (this._relatedMarkers)
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
        var dragging = this.dragging;
        if (dragging) {
            const mapPos = this._map.mouseEventToContainerPoint(e);
            // delta (stored in 'dragging') to compensate for where inside title bar drag was started
            this._setPos({x: mapPos.x - dragging[0], y: mapPos.y - dragging[1]});
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
