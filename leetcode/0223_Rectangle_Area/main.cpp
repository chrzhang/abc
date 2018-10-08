class Solution {
public:
    int computeArea(int A, int B, int C, int D, int E, int F, int G, int H) {
        // Add the areas of the rectangles and subtract the overlap
        int a1 = (C - A) * (D - B);
        int a2 = (G - E) * (H - F);
        int sum = a1 + a2;
        if (a1 && a2 && !((G <= A) || (E >= C) || (F >= D) || (H <= B))) {
            sum -= (min(C, G) - max(A, E)) * (min(H, D) - max(F, B));
        }
        return sum;
       
    }
};