var grid = null;
    
function render(model) {
    var cols = model['columns'].map(JSON.parse);
    cols.map(function(d) {d['editor'] = Slick.Editors.Text;});
    var rows = model['rows'].map(JSON.parse);
    var options = {
	editable: true,
	enableCellNavigation: true,
	asyncEditorLoading: false,
	autoEdit: false
    };
    if (!$("#grid").length) {
        /*var height = $(".output-panel").height();
        var width = $(".output-panel").width();
        var $div = $("<div>", {"id":"grid", "style": "height:" + height + "px;" + "width:" + width + "px" });
        $(".output-panel").append($div);*/
	$(".output-panel").attr("id", "grid");
	grid = new Slick.Grid("#grid", rows, cols, options);
	grid.setSelectionModel(new Slick.CellSelectionModel());
    }
    else {
        if (cols == grid.getColumns()) {
            grid.setData(rows)
            grid.render();
        }
        else {
            grid.setColumns(cols);
            grid.setData(rows);
            grid.render();
        }
    }
    grid.onCellChange.subscribe(function(e, args) {
        var row = Object.values(args.item);
        var val = row[args.cell];
        var pos = [args.row, args.cell];
        var cellInfo = {pos: pos, value: val};
	console.log(cellInfo);
	app.ports.updateCell.send(cellInfo);
    });
    grid.onClick.subscribe(function(e, args) {
        var row = args.row;
        var col = args.cell;
        var pos = [row, col];
        var cellInfo = {pos: pos, value: ""};
        console.log(cellInfo);
        app.ports.cellSelection.send(cellInfo);
    });
}

function gotoCell(cellInfo) {
    var row = cellInfo.pos[0];
    var col = cellInfo.pos[1];
    grid.gotoCell(row, col, true);
}

app.ports.render.subscribe(render);
app.ports.gotoCell.subscribe(gotoCell);
