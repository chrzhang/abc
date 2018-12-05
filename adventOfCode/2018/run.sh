#!/bin/bash

pylint3 --max-line-length=200 *.py
pep8 --max-line-length=200 *.py
python -m unittest -v aoc18
