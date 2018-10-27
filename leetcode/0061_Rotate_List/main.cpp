class Solution {
public:
    ListNode * rotateOnce(ListNode * head) {
        if (!head->next) { return head; }
        auto tail = head;
        while (tail->next->next) {
            tail = tail->next;
        }
        auto result = tail->next;
        result->next = head;
        tail->next = nullptr;
        return result;
    }
    int getSize(ListNode * head) {
        if (!head) { return 0; }
        auto curr= head;
        int c = 0;
        while (curr) {
            ++c;
            curr = curr->next;
        }
        return c;
    }
    ListNode* rotateRight(ListNode* head, int k) {
        if (k <= 0 || !head) { return head; }
        auto s = getSize(head);
        auto l = k % s;
        for (int i = 0; i < l; ++i) {
            cout << "Rotating " << i << endl;
            head = rotateOnce(head);
        }
        return head;
    }
};
