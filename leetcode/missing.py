import os
"""Print Leetcode problem #s missing from these solutions."""
dirs = [x.split('_')[0] for x in os.listdir()]
content = [int(x) for x in dirs if x.isdigit()]
print(set(range(content[-1] + 1)[1:]) - set(content))
