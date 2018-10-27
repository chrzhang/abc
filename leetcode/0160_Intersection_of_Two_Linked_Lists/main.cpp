class Solution {
public:
    int findLen(ListNode * n) {
        int r = 0;
        while (n) {
            ++r;
            n = n->next;
        }
        return r;
    }
    ListNode *getIntersectionNode(ListNode *headA, ListNode *headB) {
        int lenA = findLen(headA);
        int lenB = findLen(headB);
        ListNode * headLonger = lenA > lenB ? headA : headB;
        ListNode * headShorter = lenA > lenB ? headB : headA;
        for (int i = 0; i < abs(lenA - lenB); ++i) {
            headLonger = headLonger->next;
        }
        while (headLonger && headShorter) {
            if (headLonger == headShorter) { return headLonger; }
            headLonger = headLonger->next;
            headShorter = headShorter->next;
        }
        return nullptr;
    }
};