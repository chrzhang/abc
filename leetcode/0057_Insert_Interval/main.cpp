class Solution {
public:
vector<Interval> insert(vector<Interval> & v, const Interval iv) {
    vector<Interval> result;
    auto it = v.begin();
    for (; it < v.end() && it->start < iv.start; ++it) {}
    v.insert(it, iv);
    vector<Interval *> affected_ivs;
    for (auto & ci : v) {
        if ((ci.start >= iv.start && ci.start <= iv.end) ||
            (ci.end >= iv.start && ci.end <= iv.end) ||
            (ci.start <= iv.start && ci.end >= iv.end)) {
            affected_ivs.push_back(&ci);
        }
    }
    if (affected_ivs.empty()) { return result; }
    set<Interval *> merged_ivs;
    auto ci_it = affected_ivs.begin();
    auto ni_it = next(ci_it);
    while (ni_it != affected_ivs.end()) {
        if ((*ni_it)->start <= (*ci_it)->end) {
            (*ci_it)->end = max((*ci_it)->end, (*ni_it)->end);
            merged_ivs.insert(*ni_it);
            ni_it++;
        } else {
            ci_it = ni_it;
            ni_it++;
        }
    }
    for (auto & ci : v) {
        if (merged_ivs.find(&ci) == merged_ivs.end()) {
            result.push_back(ci);
        }
    }
    return result;
}
};