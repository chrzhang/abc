#!/bin/bash

set -euo pipefail

echo "PYLINT"

pylint --max-line-length=200 *.py

echo "PEP8"

python3 -m pycodestyle --max-line-length=200 *.py

python3 -m unittest -v aoc18
