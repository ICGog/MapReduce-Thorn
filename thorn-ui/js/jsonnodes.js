/* This script updates the presented data */

function formatNodes() {
    var listnodes = $get(); //JSON from server URL
      
    var succ;
    var fail;
    var busy;
    var map;
    var red;

    for (var n in ns) { // loop through each node
      var nodes = (listnodes.text(n));
      succ = (nodes.succ);
      fail = (nodes.fail);
      busy = (nodes.busy);
      map = (nodes.map);
      red = (nodes.red);
      // info for each node is now extracted out
    }
    
}
