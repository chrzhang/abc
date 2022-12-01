#!/usr/bin/env python3

import heapq
import doctest

with open("inputs/d01", "r") as f:
    lines = f.read().strip().split("\n")
lines.append("")

best = 0
elf_total = 0
top_3_totals = []

for line in lines:
  if line:
    elf_total += int(line)
  else:
    best = max(best, elf_total)
    if len(top_3_totals) < 3:
      heapq.heappush(top_3_totals, elf_total)
    elif top_3_totals[0] < elf_total:
      heapq.heappop(top_3_totals)
      heapq.heappush(top_3_totals, elf_total)
    elf_total = 0

def main():
  """
  Part 1
  >>> best
  71506

  Part 2
  >>> sum(top_3_totals)
  209603
  """
  print(f"Part 1: {best}")
  print(f"Part 2: {sum(top_3_totals)}")



if __name__ == "__main__":
  doctest.testmod()
  main()
