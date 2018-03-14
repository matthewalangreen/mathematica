// C program for coin change problem.
import java.util.Arrays;

void setup() {
  size(400,400);
}

// Driver program to test above function
void draw()
{
    int arr[] = {1,2,5,10,20,50,100,200};
    int m = arr.length;
    int n = 200;
    println(count(arr,m,n));
}

int count(int S[], int m, int n )
{
    int x, y;

    // We need n+1 rows as the table is constructed
    // in bottom up manner using the base case 0
    // value case (n = 0)
    int[][] table = new int[n+1][m];

    // Fill the enteries for 0 value case (n = 0)
    for (int i=0; i<m; i++)
        table[0][i] = 1;

    // Fill rest of the table entries in bottom
    // up manner
    for (int i = 1; i < n+1; i++)
    {
        for (int j = 0; j < m; j++)
        {
            // Count of solutions including S[j]
            if(i - S[j] >= 0) {
              x = table[i - S[j]][j];
            } else {
              x = 0;
            }
            // Count of solutions excluding S[j]
            if(j >= 1) {
              y = table[i][j-1];
            } else {
              y = 0;
            }

            // total count
            table[i][j] = x + y;
        }
    }
    return table[n][m-1];
    
}