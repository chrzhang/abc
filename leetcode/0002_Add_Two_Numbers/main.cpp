class Solution {
public:
    ListNode* addTwoNumbers(ListNode* l1, ListNode* l2)
    {
        ListNode* result = NULL;
        ListNode* lastNode = NULL;
        int carry = 0;
        while (l1 != NULL || l2 != NULL) {
            const int l1Digit = l1 == NULL ? 0 : l1->val;
            const int l2Digit = l2 == NULL ? 0 : l2->val;
            int sum = carry + l1Digit + l2Digit;
            carry = sum / 10;
            sum = sum - 10 * carry;
            if (result == NULL) {
                result = new ListNode(sum);
                lastNode = result;
            } else {
                ListNode* newNode = new ListNode(sum);
                lastNode->next = newNode;
                lastNode = newNode;
            }
            l1 = l1 == NULL ? l1 : l1->next;
            l2 = l2 == NULL ? l2 : l2->next;
        }
        if (carry) {
            ListNode* newNode = new ListNode(carry);
            lastNode->next = newNode;
            lastNode = newNode;
        }
        return result;
    }
};