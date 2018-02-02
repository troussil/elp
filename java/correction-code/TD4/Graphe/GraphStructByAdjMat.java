/** 
* This class represents a graph data structure by an adjacency matrix
* (there is a edge in the graph for each matrix value different from 0)
*/
public class GraphStructByAdjMat extends SquareMatrix implements GraphStruct {

  /** set of nodes */
  private Node[] myNodeSet; 

  /** Constructor
   * @param adjacencySquareMatrix adjacency matrix 
   * (must be a symmetric square matrix)
   */
  public GraphStructByAdjMat(SquareMatrix adjacencyMatrix) {
    //copy of the adjacency matrix
    super(adjacencyMatrix);
    if (!adjacencyMatrix.isSymmetric()) 
      throw new RuntimeException("Adjacency matrix must be symmetric.");

    //construction of the node set
    myNodeSet = new Node[N]; 
    for (int i = 0; i < myNodeSet.length; i++) {
      myNodeSet[i] = new Node(i); 
    }
  }

  /**
   * Getting node set
   * @return node set
   */
  public Node[] getNodeSet() {
    return myNodeSet; 
  }

  /** 
   * Getting neighbors from a given node 
   * @param aNode an arbitrary node of the node set
   * @return neighbor set
   */
  public Node[] getNeighbors(Node aNode) {
    //count
    int c = 0; 
    for (int j = 0; j < N; j++) {
      if (data[aNode.id][j] != 0)
	c++; 
    }
    //copy
    Node[] neighbors = new Node[c];
    c = 0; 
    for (int j = 0; j < this.N; j++) {
      if (data[aNode.id][j] != 0) {
	neighbors[c] = myNodeSet[j]; 
	c++; 
      }
    }
    return neighbors; 
  }
}