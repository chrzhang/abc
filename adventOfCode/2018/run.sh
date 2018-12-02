#!/bin/bash

pylint *.py
pep8 *.py
python -m unittest -v aoc18
