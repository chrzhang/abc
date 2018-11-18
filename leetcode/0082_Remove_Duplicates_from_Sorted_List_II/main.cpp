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
    ListNode* deleteDuplicates(ListNode* head)
    {
        if (!head)
            return head;
        ListNode* result_head = NULL;
        ListNode* result_tail = NULL;
        // We will be walking 3 side-by-side pointers across and considering the middle of the 3
        // To examine every node as the middle of 3, we will need 1 begin and 1 end sentinel nodes
        ListNode first_sentinel(0);
        first_sentinel.next = head;
        ListNode* temp = head;
        while (temp->next) {
            temp = temp->next;
        }
        ListNode end_sentinel(0);
        temp->next = &end_sentinel;
        ListNode* l = &first_sentinel;
        ListNode* m = l->next;
        ListNode* r = m->next;
        while (l && m && r) {
            bool mid_differs_from_sides = false;
            if ((l == &first_sentinel || l->val != m->val) && (r == &end_sentinel || r->val != m->val)) {
                if (result_head == NULL) {
                    result_head = m;
                } else {
                    result_tail->next = m;
                }
                result_tail = m;
                m->next = NULL;
                l->next = r;
                m = r;
                r = r->next;
            } else {
                l = l->next;
                m = m->next;
                r = r->next;
            }
        }
        if (result_tail)
            result_tail->next = NULL;
        ListNode* curr = first_sentinel.next;
        while (curr && curr->next) {
            auto t = curr->next;
            delete curr;
            curr = t;
        }
        return result_head;
    }
};
