var ht = null;
    
function render(model) {
    var cols = model['columns'];
    var rows = model['rows'];
    console.log(cols);
    console.log(rows);
    if (!$("#grid").length) {
        $("<div/>", {id : "grid"}).appendTo(".output-panel");
        var container = document.getElementById("grid");
	ht = new Handsontable(container, {
            data: rows,
            minSpareRows: 1,
            minSpareCols: 1,
            rowHeaders: true,
            colHeaders: true,
            contextMenu: true
        });
        ht.updateSettings({
            afterChange: function(changes, source) {
                var realChange = []
                for (var i = 0; i < changes.length; i++) {
                    if (changes[i][2] != changes[i][3])
                        realChange.push(changes[i]);
                }
                if (realChange.length) {
                    var arr = realChange[0]; //need to be changed later
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
        data = []
        for (var i = 0; i < rows.length; i++)
            for (var j = 0; j < rows[i].length; j++)
                data.push([i, j, rows[i][j]]);
        ht.setDataAtCell(data);
        ht.render();
    }
}

app.ports.render.subscribe(render);
