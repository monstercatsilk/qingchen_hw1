#include <stdio.h>
#include <math.h>
#include "algebra.h"
Matrix create_matrix(int row, int col)
{
    Matrix m;
    m.rows = row;
    m.cols = col;
    return m;
}

Matrix add_matrix(Matrix a, Matrix b)
{
    // ToDo
    Matrix matrix; 
    int i,j;   
    if(a.rows == b.rows && a.cols == b.cols){
        matrix.rows = a.rows;
        matrix.cols = a.cols;
        for(i = 0;i < matrix.rows;i++){
            for(j = 0;j < matrix.cols;j++){
                matrix.data[i][j] = a.data[i][j] + b.data[i][j];
            }
        }
        return matrix;
    }
    else{
        printf("Error: Matrix a and b must have the same rows and cols.\n");
        return create_matrix(0, 0);
    }
    
}

Matrix sub_matrix(Matrix a, Matrix b)
{
    // ToDo
    Matrix matrix;    
    int i,j;
    if(a.rows == b.rows && a.cols == b.cols){
        matrix.rows = a.rows;
        matrix.cols = a.cols;
        for(i = 0;i < matrix.rows;i++){
            for(j = 0;j < matrix.cols;j++){
                matrix.data[i][j] = a.data[i][j] - b.data[i][j];
            }
        }
        return matrix;
    }
    else{
        printf("Error: Matrix a and b must have the same rows and cols.\n");
        return create_matrix(0, 0);
    }
    
}

Matrix mul_matrix(Matrix a, Matrix b)
{
    // ToDo
    Matrix matrix;
    int i,j,k;
    if(a.cols == b.rows){
        matrix.rows = a.rows;
        matrix.cols = b.cols;
        for(i = 0;i < matrix.rows;i++){
            for(j = 0;j < matrix.cols;j++){
                matrix.data[i][j]=0;
            }
        }
        for(i = 0;i < matrix.rows;i++){
            for(j = 0;j < matrix.cols;j++){
                for(k = 0;k < a.cols;k++){
                    matrix.data[i][j]+=a.data[i][k] * b.data[k][j];
                }
            }
        }
        return matrix;
    }
    else{
        printf("Error: The number of cols of matrix a must be equal to the number of rows of matrix b.\n");
        return create_matrix(0, 0);
    }
    
}

Matrix scale_matrix(Matrix a, double k)
{
    // ToDo
    Matrix matrix;
    int i,j;
    matrix.rows = a.rows;
    matrix.cols = a.cols;
    for(i = 0;i < matrix.rows;i++){
        for(j = 0;j < matrix.cols;j++){
            matrix.data[i][j] = k * a.data[i][j];
        }
    }
    return matrix;
}

Matrix transpose_matrix(Matrix a)
{
    // ToDo
    Matrix matrix;
    int i,j;
    matrix.rows = a.cols;
    matrix.cols = a.rows;
    for(i = 0;i < matrix.rows;i++){
        for(j = 0;j < matrix.cols;j++){
            matrix.data[i][j] = a.data[j][i];
        }
    }
    return matrix;
}

double det_matrix(Matrix a)
{
    // ToDo
    double det = 0;
    int i,j,k;
    if(a.rows == a.cols){
        if(a.rows == 1){
            det = a.data[0][0];
        }
        else{
            for(i = 0;i < a.rows;i++){
                Matrix matrix;
                matrix.rows = a.rows - 1;
                matrix.cols = a.cols - 1;
                for(j = 0;j < matrix.rows;j++){
                    for(k = 0;k < matrix.cols;k++){
                        if(k < i){
                            matrix.data[j][k] = a.data[j+1][k];
                        }
                        else{
                            matrix.data[j][k] = a.data[j+1][k+1];
                        }
                    }
                }
                det+=det_matrix(matrix) * a.data[0][i] * (i % 2 == 0? 1 : -1);
            }
        }
        return det;
    }
    else{
        printf("Error: The matrix must be a square matrix.\n");
        return 0;
    }
}

Matrix inv_matrix(Matrix a)
{
    // ToDo
    Matrix matrix;
    int i,j,p,q;
    if(fabs(det_matrix(a)) < 1e-10){
        printf("Error: The matrix is singular.\n");
        return create_matrix(0, 0);
    }
    else if(a.rows != a.cols){
        printf("Error: The matrix must be a square matrix.\n");
        return create_matrix(0, 0);
    }
    else{
        matrix.rows = a.rows;
        matrix.cols = a.cols;
        for(i = 0;i < matrix.rows;i++){
            for(j = 0;j < matrix.cols;j++){
                Matrix temp;
                temp.rows = matrix.rows - 1;
                temp.cols = matrix.cols - 1;
                for(p = 0;p < temp.rows; p++){
                    for(q = 0;q < temp.cols;q++){
                        if(p < i){
                            if(q < j){
                                temp.data[p][q] = a.data[p][q];
                            }else{
                                temp.data[p][q] = a.data[p][q+1];
                            }
                        }
                        else{
                            if(q < j){
                                temp.data[p][q] = a.data[p+1][q];
                            }else{
                                temp.data[p][q] = a.data[p+1][q+1];
                            }
                        }
                    }
                }
                matrix.data[j][i] = det_matrix(temp) / det_matrix(a) * ((i + j) % 2 == 0? 1 : -1);
            }
        }
        return matrix;
    }
}

int rank_matrix(Matrix a)
{
    // ToDo
    int m = a.rows, n = a.cols, i, j, k, l;
    double temp;
    int all_zero = 1;
    for (i = 0; i < a.rows; i++) {
        for (j = 0; j < a.cols; j++) {
            if (fabs(a.data[i][j]) > 1e-10) all_zero = 0;
        }
    }
    if (all_zero) return 0;

    Matrix b;
    b.rows = m;
    b.cols = n;
    for(i = 0;i < m;i++){
        for(j = 0;j < n;j++){
            b.data[i][j] = a.data[i][j];
        }
    }

    for(i = 0;i < m && i < n;i++){
        j = i;
        while(fabs(b.data[j][i]) < 1e-10 && j < m){
            j++;
        }
        if(j == m){
            for(l = 0;l < m;l++){
                
                b.data[l][i] = b.data[l][n-1];
                
            }
            i--;
            n--;
            continue;
        }
        for(k = 0;k < n;k++){
            temp = b.data[i][k];
            b.data[i][k] = b.data[j][k];
            b.data[j][k] = temp;
        }
        
        for(k = i + 1;k < m;k++){
                for(j = n-1;j >= 0;j--){

                    b.data[k][j] = b.data[k][j] - b.data[i][j] / b.data[i][i] * b.data[k][i];
                }
        }
        
        
    }
    i = 0;
    while(fabs(b.data[i][i]) > 1e-10 && i < m && i < n) i++;
    return i;
}

double trace_matrix(Matrix a)
{
    // ToDo
    int i;
    double trace = 0;
    if(a.rows == a.cols){
        for(i = 0;i < a.rows;i++){
            trace+=a.data[i][i];
        }
        return trace;
    }
    else{
        printf("Error: The matrix must be a square matrix.\n");
        return 0;
    }
        
}

void print_matrix(Matrix a)
{
    for (int i = 0; i < a.rows; i++)
    {
        for (int j = 0; j < a.cols; j++)
        {
            // 按行打印，每个元素占8个字符的宽度，小数点后保留2位，左对齐
            printf("%-8.2f", a.data[i][j]);
        }
        printf("\n");
    }
}