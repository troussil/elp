/******************************************************************************
 *  Inspire de 
 *  http://introcs.cs.princeton.edu/java/95linear/Matrix.java
 ******************************************************************************/

class SquareMatrix {
    protected final int N;          // number of rows and columns
    protected final int[][] data;   // N-by-N array

    // create N-by-N matrix of 0's
    public SquareMatrix(int N) {
        this.N = N;
        data = new int[N][N];
    }

    // create matrix based on 2d array
    public SquareMatrix(int[][] data) {
        N = data.length;
        this.data = new int[N][N];
        for (int i = 0; i < N; i++)
            for (int j = 0; j < N; j++)
                    this.data[i][j] = data[i][j];
    }

    // copy constructor
    protected SquareMatrix(SquareMatrix A) { this(A.data); }

    // create and return the N-by-N identity matrix
    public static SquareMatrix identity(int N) {
        SquareMatrix I = new SquareMatrix(N);
        for (int i = 0; i < N; i++)
            I.data[i][i] = 1;
        return I;
    }

    public boolean isSymmetric() {
	boolean symmetric = true; 
        for (int i = 0; i < N; i++)
            for (int j = 0; j < N; j++)
                symmetric = symmetric && (data[i][j] == data[j][i]);
	return symmetric; 
    }

    // create and return the transpose of the invoking matrix
    public SquareMatrix transpose() {
        SquareMatrix A = new SquareMatrix(N);
        for (int i = 0; i < N; i++)
            for (int j = 0; j < N; j++)
                A.data[j][i] = this.data[i][j];
        return A;
    }

    // return C = A + B
    public SquareMatrix plus(SquareMatrix B) {
        SquareMatrix A = this;
        if (B.N != A.N) throw new RuntimeException("Illegal matrix dimensions.");
        SquareMatrix C = new SquareMatrix(N);
        for (int i = 0; i < N; i++)
            for (int j = 0; j < N; j++)
                C.data[i][j] = A.data[i][j] + B.data[i][j];
        return C;
    }


    // return C = A - B
    public SquareMatrix minus(SquareMatrix B) {
        SquareMatrix A = this;
        if (B.N != A.N) throw new RuntimeException("Illegal matrix dimensions.");
        SquareMatrix C = new SquareMatrix(N);
        for (int i = 0; i < N; i++)
            for (int j = 0; j < N; j++)
                C.data[i][j] = A.data[i][j] - B.data[i][j];
        return C;
    }

    // does A = B exactly?
    public boolean eq(SquareMatrix B) {
        SquareMatrix A = this;
        if (B.N != A.N) throw new RuntimeException("Illegal matrix dimensions.");
        for (int i = 0; i < N; i++)
            for (int j = 0; j < N; j++)
                if (A.data[i][j] != B.data[i][j]) return false;
        return true;
    }

    // return C = A * B
    public SquareMatrix times(SquareMatrix B) {
        SquareMatrix A = this;
        if (A.N != B.N) throw new RuntimeException("Illegal matrix dimensions.");
        SquareMatrix C = new SquareMatrix(A.N);
        for (int i = 0; i < C.N; i++)
            for (int j = 0; j < C.N; j++)
                for (int k = 0; k < A.N; k++)
                    C.data[i][j] += (A.data[i][k] * B.data[k][j]);
        return C;
    }


    // print matrix to standard output
    public void show() {
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++) 
                System.out.print(data[i][j]);
            System.out.println();
        }
    }

}
