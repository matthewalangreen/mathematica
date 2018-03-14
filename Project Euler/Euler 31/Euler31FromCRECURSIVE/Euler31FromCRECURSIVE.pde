// C program for coin change problem.
import java.util.Arrays;



// Driver program to test above function
void draw()
{
    println(ways(200,7));
}

 int coins[] = {1,2,5,10,20,50,100,200};
 int amount = 200;
 int[] memo = new int[amount+1];
 
   
void setup() {
  size(400,400);
  for (int i=0; i<= amount; i++)
   memo[i] = 0;
}
 
 
 int memoWays() {
   return 0;
 }
 
 int ways(int target, int avc) {
   if (avc < 1) {
    return 1; 
   }
   int res = 0;
   while(target >= 0) {
    res = res + ways(target, avc-1);
    target = target - coins[avc];
   }
   return res;
 }




    