class Solution {
public:
    struct ListNodeCmp {
        bool operator()(const ListNode* lhs, const ListNode* rhs) const
        {
            return lhs->val < rhs->val;
        }
    };

    ListNode* mergeKLists(vector<ListNode*>& v)
    {
        ListNode *rh, *rt;
        rh = rt = nullptr;
        multiset<ListNode*, ListNodeCmp> s;
        for (ListNode* l : v) {
            if (l == nullptr)
                continue;
            s.insert(l);
        }
        while (!s.empty()) {
            ListNode* smallest = *s.begin();
            assert(smallest != nullptr);
            s.erase(s.begin());
            if (smallest->next != nullptr) {
                s.insert(smallest->next);
            }
            if (rh == NULL) {
                rh = rt = smallest;
            } else {
                rt->next = smallest;
                rt = smallest;
            }
        }
        return rh;
    }
};