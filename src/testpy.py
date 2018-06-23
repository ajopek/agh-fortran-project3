#!/usr/python
import sys
import numpy as np
from library import library as l

def test_mm(N):
    matrix1 = np.ndarray(shape=(N, N), dtype=float, order='F')
    matrix2 = np.ndarray(shape=(N, N), dtype=float, order='F')

    for i in range(N):
        for j in range(N):
            matrix1[i][j] = 1
            matrix2[i][j] = 2

    multiply = l.mm(matrix1, matrix2)
    print(multiply)

def test_gaussian(N):
    h = 1.0 / (N * N)
    a1 = 1.0 / h
    a2 = -2.0 / h

    matrixA = np.ndarray(shape=(N, N), dtype=float, order='F')
    X = np.ndarray(shape=(N), dtype=float, order='F')

    for i in range(N): 
        for j in range(N): A[i][j] = 0.0

    for i in range(N):
        matrixA[i][i] = a2
        matrixA[i] = 0
        if i != 0:
            matrixA[i][i - 1] = a1
        if i != N-1:
            matrixA[i][i + 1] = a1 

    matrixA[N-1] = 1

    A, matrixA = l.gaussian_elimination(A, X)
    matrixA = matrixA * (N * (N + 1) * (-1))
    print(matrixA)

_N = int(sys.argv[1])
test_mm(_N)
test_gaussian(_N)
