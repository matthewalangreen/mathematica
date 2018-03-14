// C program for coin change problem.
import java.util.Arrays;

// Driver program to test above function
void draw()
{
    println(memoWays(200,7));
}

 int coins[] = {1,2,5,10,20,50,100,200};
 int amount = 200;
 //int[][] memo = new int[amount+1][coins.length+1];
 int[][] memo = new int[amount+1][coins.length];
 
   
void setup() {
  size(400,400);
  for (int i=0; i< amount; i++)
   for (int j=0; j<coins.length; j++)
     memo[i][j]=0;
}
 
 int memoWays(int target, int avc) {
   if(avc < 1) {
    return 1; 
   }
   
   int t = target;
   if(memo[t][avc] > 0) {
    return memo[t][avc]; 
   }
   
   int res = 0;
   while(target >= 0) {
    res = res + memoWays(target, avc-1);
    target = target - coins[avc];
   }
   memo[t][avc] = res;
   return res;
 }
 



    