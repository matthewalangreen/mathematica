// From: https://www.geeksforgeeks.org/dynamic-programming-set-7-coin-change/

import java.util.Arrays;

int arr[] = {1,2,5,10,20,50,100,200};
int m = arr.length;
int n = 200;

void setup() {
  size(800,800);
}

void draw() {
   println(countWays(arr,m,n)); 
}
int countWays(int S[], int m, int n)
{
    //Time complexity of this function: O(mn)
    //Space Complexity of this function: O(n)
 
    // table[i] will be storing the number of solutions
    // for value i. We need n+1 rows as the table is
    // constructed in bottom up manner using the base
    // case (n = 0)
    int[] table = new int[n+1];
 
    // Initialize all table values as 0
    Arrays.fill(table, 0);   //O(n)
 
    // Base case (If given value is 0)
    table[0] = 1;
 
    // Pick all coins one by one and update the table[]
    // values after the index greater than or equal to
    // the value of the picked coin
    for (int i=0; i<m; i++)
        for (int j=S[i]; j<=n; j++)
            table[j] += table[j-S[i]];
 
    return table[n];
}

 
  