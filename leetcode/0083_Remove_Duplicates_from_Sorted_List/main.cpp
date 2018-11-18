/**
 * Definition for singly-linked list.
 * struct ListNode {
 *     int val;
 *     ListNode *next;
 *     ListNode(int x) : val(x), next(NULL) {}
 * };
 */
class Solution {
public:
    ListNode* deleteDuplicates( ListNode* head) {
        if (!head) return NULL;
        auto prev = head;
        auto curr = prev->next;
        while (curr) {
            if (curr->val == prev->val) {
                auto temp = curr->next;
                delete curr;
                curr = temp;
                prev->next = curr;
            } else {
                curr = curr->next;
                prev = prev->next;
            }
        }
        return head;
    }
};
