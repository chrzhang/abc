#!/usr/local/bin/python

import json

def sumNums(o):
    if type(o) is int:
        return o
    if type(o) is list:
        return sum([sumNums(e) for e in o])
    if type(o) is dict:
        if 'red' in o.values():
            return 0
        else:
            return sumNums(list(o.values()))
    else:
        return 0

with open('input.txt', 'rb') as f:
    obj = json.loads(f.read())
    print(sumNums(obj))
