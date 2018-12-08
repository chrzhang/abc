#!/bin/bash

echo "PYLINT"

pylint3 --max-line-length=200 *.py

echo "PEP8"

python3 -m pycodestyle --max-line-length=200 *.py

python3 -m unittest -v aoc18
