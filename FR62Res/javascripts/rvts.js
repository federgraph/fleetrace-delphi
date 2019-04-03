//constructor
function TableSort(tab)
{
    this.g_pattern = new RegExp("(^| )g(\\d)+( |$)");
    
    this.upchar = String.fromCharCode(160,9650);
    this.downchar = String.fromCharCode(160,9660);
    this.spacechar = ''; //String.fromCharCode(160,160,160,160);

    this.firstPass = 1;
    this.cc = 0; //current column
            
	this.tbody = tab.getElementsByTagName("tbody")[0];
	var thead = tab.getElementsByTagName("thead");
	if (thead.length == 0)
	    return;
	    
	this.ths = tab.getElementsByTagName("thead")[0].getElementsByTagName("tr")[0].getElementsByTagName("th");
	this.trs = this.tbody.getElementsByTagName("tr");

	this.rowCount = this.trs.length;
	this.colCount = this.ths.length;		
	
	this.grid = new Array(this.rowCount);
    
	this.sortType = new Array(this.colCount); 
	for(var s = 0; s < this.colCount; s++) 
	    this.sortType[s] = "u";

    this.sort = TableSort_Sort;
    
    for(var i = 0; i < this.ths.length; i++) 
    {
        var th = this.ths[i];
        th.colIndex = i;
        th.style.cursor = "pointer";               
        th.tableSort = this;
        
        //attach a click-event to the <th>-element
        th.onclick = function() {
            //this.thisObject.sort(this.colIndex);          
            var temp = this.tableSort;
            var tempIndex = this.colIndex;                    
            if (tempIndex > -1)
            {                    
                if (temp != null)
                    temp.sort(tempIndex); 
                else    
                    alert("temp = null");
            }                                         
         }
                        
        //create invisible phantom elements for holding the sort indicator char
        th.appendChild(document.createTextNode(this.spacechar)); 
        th.lastChild.data = this.spacechar;
        this.cc = i;
    }         
}
        
function TableSort_Sort(col) 
{   
    var cc = this.cc;
    var tds, td, val, css, idx, gcol;
     
    if (this.firstPass == 1) 
    {
        //on firstPass run through all rows,
        //buffer the whole content of the table in grid[row, column] 
        //and initialize the sortType for each column
        for(var r = 0; r < this.rowCount; r++) 
        {
            tds = this.trs[r].getElementsByTagName("td");
            this.grid[r] = new Array(this.colCount);
            
            for(var c = 0; c < this.colCount; c++) 
            {
            	td = tds[c];
            	
                val = td.firstChild.nodeValue;
                css = td.className;
                idx = git[r][c];                                
				gcol = this.g_pattern.test(css);				    
                                
                this.grid[r][c] = new Array(4);
                                
                this.grid[r][c][0] = val;
                this.grid[r][c][1] = css;
                this.grid[r][c][2] = idx;
                this.grid[r][c][3] = gcol;

            }  
        }
    }
        
    var sf; //sort_char_field
    
    sf = this.ths[this.cc].lastChild;
    //sort the grid (backend data container)
    if(col == cc && this.firstPass == 0) 
    { 
        //swap the sort indicator
        if ( sf.data == this.downchar )
            sf.data = this.upchar;
        else
            sf.data = this.downchar;
    }    
    else
    { 
        //remove the sort indicator from the old sort column
        if ( cc >= 0 && cc < this.colCount ) 
            sf.data = this.spacechar;
            
        //update current column index variables
        this.cc = col;
        sf = this.ths[this.cc].lastChild;
        
        //and update the sort indicator of the now current column
        sf.data = this.upchar; //do not use sf, because this.cc has changed
    }
      
    //copy over data from the backend data store to the visible html table cells
    var desc = sf.data == this.downchar;
    var r;
    for(var row = 0; row < this.rowCount; row++) 
    {
        //swap direction if desc
        if (desc)
          r = this.rowCount - 1 - row;        
        else
          r = row;
        
        tds = this.trs[r].getElementsByTagName("td");
        for(var c = 0; c < this.colCount; c++) 
        {        	
        	td = tds[c];
        	idx = this.grid[row][col][2] - 1;
        	css = this.grid[idx][c][1];
        	val = this.grid[idx][c][0];
        	gcol = this.grid[idx][c][3];
        	         	
            td.firstChild.nodeValue = val;
            if (gcol)
                td.className = css;
        }
    }            
    
    this.firstPass = 0;        
}

var tableSorter =
{
    init: function() {
        if (!tableSorter.browserTest())
            return;
        var tabs = tableSorter.getTables();
        for (var i = 0; i < tabs.length; i++)
            new TableSort(tabs[i]);

        git = tableSorter.getIndexTable();
    },

    browserTest: function() {
        var domEnabled = document.getElementsByTagName;
        if (domEnabled)
            domEnabled = document.getElementsByTagName('body')[0].appendChild;
        if (!domEnabled)
            return false;
        return true;
    },

    getTables: function() {
        var allTables = document.getElementsByTagName("table");
        var sortableTables = new Array();
        for (var i = 0; i < allTables.length; i++) {
            if (Core.hasClass(allTables[i], "sortable"))
                sortableTables[sortableTables.length] = allTables[i];
        }
        return sortableTables;
    },

    getIndexTable: function() {
        var indexTableDiv = document.getElementById("index_table");
        if (indexTableDiv) {
            var jsonText = indexTableDiv.firstChild.innerHTML;
            var jsonObject = eval('(' + jsonText + ')');
            return jsonObject;
        }
        return false;
    }

}  
  	
var git; //global_index_table;  

Core.start(tableSorter);
