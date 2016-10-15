package com.testsolutions.BackTrack;

/**
 * Created by jainu on 14/10/16.
 */
public class MySudokuSolver
{
    final static int N = 9;
    public static void main(String args[])
    {
        int a[][] = new int[][]{
                {1, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 2, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 3, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 4, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 5, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 6, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 7, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 8, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 9}
            };


        if( solveSudoku(a) ){
            print(a);
            return;
        }
        print(a);
        System.out.println("No solution found");
    }


    private static boolean solveSudoku(int[][] a)
    {

        MyCoordinates unassignedSquare = findUnassignedSquare(a);
        if( unassignedSquare == null)
        {
            System.out.println("unassigned : null");
            return true;
        }
        System.out.println("unassigned :" +unassignedSquare.toString());

        for(int num=1; num<=N; num++)
        {
            if( isSafeToPlace(a, unassignedSquare, num) )   // check if a number from 1 to N can be placed at given square
            {
                place(a, unassignedSquare, num);
                if ( solveSudoku(a) ) return true;
                unplace(a, unassignedSquare);
                System.out.println("unplace :" +unassignedSquare.toString());
            }
        }
        return false;   // this will backtrack
    }

    private static void place(int[][] a, MyCoordinates myCoordinates, int num)
    {
        a[myCoordinates.getRow()][myCoordinates.getCol()] = num;
    }

    private static void unplace(int[][] a, MyCoordinates myCoordinates)
    {
        a[myCoordinates.getRow()][myCoordinates.getCol()] = 0;
    }

    private static boolean isSafeToPlace(int[][] a, MyCoordinates myCoordinates, int num) {
        return ( isSafeinRow(a, myCoordinates.getRow(), num)
                && isSafeinCol(a, myCoordinates.getCol(), num)
                && isSafeinBox(a, myCoordinates.getRow(), myCoordinates.getCol(), num) );
    }

    private static boolean isSafeinBox(int[][] a, int row, int col, int num)
    {
        int startRow = row - row%3;
        int startCol = col - col%3;

        for( int i=startRow; i<startRow+3; i++)
            for( int j=startCol; j<startCol+3; j++)
                if(a[i][j] == num) return false;

        return true;

    }

    private static boolean isSafeinRow(int[][] a, int row, int num)
    {
        for( int col=0; col<N; col++ )
            if(a[row][col] == num)
                return false;
        return true;
    }

    private static boolean isSafeinCol(int[][] a, int col, int num)
    {
        for( int row=0; row<N; row++ )
            if(a[row][col] == num)
                return false;
        return true;
    }

    private static MyCoordinates findUnassignedSquare(int[][] a)
    {
        for( int row=0; row<N; row++ ) {
            for (int col = 0; col < N; col++) {
                if (a[row][col] == 0) {
                    MyCoordinates coordinates = new MyCoordinates(row, col);
                    return coordinates;
                }
            }
        }

        return null;
    }

    private static void print(int[][] a) {
        for(int row=0; row<N; row++)
        {
            for(int col=0; col<N; col++)
                System.out.print(a[row][col] + " ");
            System.out.println();
        }
    }
}
