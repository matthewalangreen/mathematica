

int arr[] = {1,2,5};
int m = arr.length;
int n = 5;

//println(countWays(arr,m,n));

int countWays(int S[], int m, int n)
{
    int[] t = new int[n+1];

    t[0] = 1;

    // let S[] be repped by t[] now
    for (int i=0; i<m; i++)
        for (int j=arr[i]; j<=n; j++)
            t[j] += t[j-arr[i]];

    return t[n];
}
