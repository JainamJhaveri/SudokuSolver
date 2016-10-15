package com.testsolutions.BackTrack;

class MyCoordinates {
    private int row, col;

    public MyCoordinates(int row, int col) {
        this.row = row;
        this.col = col;
    }

    public int getRow() {
        return row;
    }

    public int getCol() {
        return col;
    }

    @Override
    public String toString() {
        return this.getRow() + ", " + this.getCol();
    }
}
