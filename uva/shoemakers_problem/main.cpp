#include <iostream>
#include <cassert>
#include <stdexcept>
#include <algorithm>
#include <vector>

using namespace std;

struct Job {
    int id;
    int durationInDays;
    int costPerDay;
    Job(int id, int durationInDays, int costPerDay)
    : id(id), durationInDays(durationInDays), costPerDay(costPerDay) {
    }
    Job(int id)
    : id(id), durationInDays(-1), costPerDay(-1) {
    }
};

bool operator<(const Job & jobA, const Job & jobB) {
    return jobA.costPerDay / (double) jobA.durationInDays >
           jobB.costPerDay / (double) jobB.durationInDays;
}

bool operator==(const Job & jobA, const Job & jobB) {
    return jobA.id == jobB.id;
}

int totalCostOfJobOrder(const vector<Job> & jobOrder) {
    int totalCost = 0;
    for (size_t currJobI = 0; currJobI < jobOrder.size(); ++currJobI) {
        const Job & currJob = jobOrder[currJobI];
        for (size_t otherJobI = currJobI + 1; otherJobI < jobOrder.size(); ++otherJobI) {
            const Job & otherJob = jobOrder[otherJobI];
            totalCost += currJob.durationInDays * otherJob.costPerDay;
        }
    }
    return totalCost;
}

void outputJobOrder(const vector<Job> & jobOrder) {
    for (const auto & job : jobOrder) {
        cout << job.id << " ";
    }
    cout << endl;
}


int main() {
    // Intuition suggests minimizing cost means knocking out the most expensive
    // jobs first but our definition of expensive needs to be cost per day / day
    // not cost per day alone
    {
        vector<Job> jobOrder({
            Job(1, 3, 4),
            Job(2, 1, 1000),
            Job(3, 2, 2),
            Job(4, 5, 5)
        });
        stable_sort(jobOrder.begin(), jobOrder.end());
        assert(jobOrder == vector<Job>({
            Job(2),
            Job(1),
            Job(3),
            Job(4)
        }));
    }
    {
        vector<Job> jobOrder({
            Job(1, 1, 1),
            Job(2, 2, 2),
            Job(3, 3, 3)
        });
        stable_sort(jobOrder.begin(), jobOrder.end());
        assert(jobOrder == vector<Job>({
            Job(1),
            Job(2),
            Job(3)
        }));
    }
    {
        vector<Job> jobOrder({
            Job(1, 3, 2),
            Job(2, 3, 5),
            Job(3, 9999, 9998),
            Job(4, 2, 3),
            Job(5, 10000, 10000)
        });
        stable_sort(jobOrder.begin(), jobOrder.end());
        assert(jobOrder == vector<Job>({
            Job(2),
            Job(4),
            Job(5),
            Job(3),
            Job(1)
        }));
    }
    {
        vector<Job> jobOrder({
            Job(1, 123, 123),
            Job(2, 3, 3),
            Job(3, 5, 5)
        });
        stable_sort(jobOrder.begin(), jobOrder.end());
        assert(jobOrder == vector<Job>({
            Job(1),
            Job(2),
            Job(3)
        }));
    }
}
