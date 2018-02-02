/**
 * Interface for graph data structures
 */
interface GraphStruct {

    /**
     * Method that provides the neighbors of a given node
     * @param aNode any node of the node set
     * @return neighbors as an array of node
     */
    Node[] getNeighbors(Node aNode); 

    /**
     * Method that provides the node set
     * @return node set as an array of node
     */
    Node[] getNodeSet(); 

}