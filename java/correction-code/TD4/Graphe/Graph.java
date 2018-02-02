import java.util.HashSet; 
import java.util.Set; 
/**
 * Class that represents a simple graph, ie
 * without edge orientation, without weight.
 * It is based on an interface for graph data
 * structures. 
 */
class Graph {

    /** graph data structure */
    private GraphStruct data; 
    
    /**
     * Constructor
     * @param data 
     */
    public Graph(GraphStruct data) {
	this.data = data; 
    }

    /** 
     * depth-first search to mark all nodes of the same connected component
     * @param node any starting node
     * @param notVisitedNodeSet set of visited nodes
     */
    public void depthFirstSearch(Node node, Set<Node> notVisitedNodeSet) {
	//mark as visited
	notVisitedNodeSet.remove(node); 
	//recursive call for all not visited neighbors
	Node[] neighbors = data.getNeighbors(node); 
	for (int i = 0; i < neighbors.length; i++) {
	    if (notVisitedNodeSet.contains(neighbors[i]))
		depthFirstSearch(neighbors[i], notVisitedNodeSet); 
	}
    }

    /**
     * @return the number of connected components
     */
    public int getNumberOfConnectedComponents() {
	//set of nodes that have been not visited yet
	HashSet<Node> notVisitedNodeSet = new HashSet<Node>(); 
	Node[] nodeSet = data.getNodeSet();
	for (int i = 0; i < nodeSet.length; i++)
	    notVisitedNodeSet.add(nodeSet[i]); 
	//connected component count
	int c = 0; 
	Node startingNode; 
	//while there are not visited nodes
	while (!notVisitedNodeSet.isEmpty()) {
	    startingNode = notVisitedNodeSet.iterator().next();  
	    depthFirstSearch(startingNode, notVisitedNodeSet);
	    c++; 
	}
	return c; 
    }

}