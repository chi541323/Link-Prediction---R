Table table,tableP1,tableP2,newTable;
int[] node = new int [27770];
int[] sourceNode = new int [154837];
int[] targetNode = new int [154837];
int i = 0;
TableRow newRow;
void setup() {
  table = loadTable("node_information.csv","header");
  tableP1 = loadTable("Period1.csv","header");
  
  newTable = new Table();
  newTable.addColumn("id");

  for (TableRow row : table.rows()) {   
    node[i] = row.getInt("id");
    newRow = newTable.addRow();
    newRow.setInt("id", node[i]);
    i++;
  }
  saveTable(newTable, "new.csv");
  i = 0;
   for (TableRow row : tableP1.rows()) {   
    sourceNode[i] = row.getInt("source id");
    targetNode[i] = row.getInt("target id");
    i++;
  }
  i = 0;
  
  
 
}