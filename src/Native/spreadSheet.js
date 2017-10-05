var ht = null;

/* this is a hack to get handsontable render all columns */
function padding(data, len) {
    for (var i = 0; i < data.length; i++) {
        var to_pad = len - data[i].length;
        data[i] = data[i].concat(Array(to_pad).fill(null));
    }
}

function render(model) {
    var header = model['header'];
    var data = model['data'];
    var max_col_len = Math.max.apply(null, data.map(function(d) {return d.length;}));
    padding(data, max_col_len);
    console.log(data);
    var height = $(".output-panel").height();
    var width = $(".output-panel").width();
    if (!$("#grid").length) {
        $("<div/>", {id : "grid"}).appendTo(".output-panel");
        var container = document.getElementById("grid");
	ht = new Handsontable(container, {
            data: data,
            width: width,
            height: height,
            minSpareRows: 1,
            minSpareCols: 1,
            rowHeaders: header ? header : true,
            colHeaders: true,
            contextMenu: true
        });
        ht.updateSettings({
            afterChange: function(changes, source) {
                if (changes && changes.length) {
                    console.log(changes);
                    var arr = changes[0]; //need to be changed later
                    var pos = [arr[0], arr[1]];
                    var cellInfo = {pos: pos, value: arr[3]};
                    console.log(cellInfo);
                    app.ports.updateCell.send(cellInfo);
                }
            },
            afterOnCellMouseDown: function(e, coords) {
                var pos = [coords.row, coords.col];
                var cellInfo = {pos: pos, value: ""};
                console.log(cellInfo);
                app.ports.cellSelection.send(cellInfo);
            }
        });
    }
    else {
        ht.updateSettings({
            data: data
        });
        ht.render();
    }
}

app.ports.render.subscribe(render);
