#!/bin/bash

set -euo pipefail

echo "PYLINT"

pylint --max-line-length=200 --max-module-lines=2000 *.py

echo "PEP8"

python3 -m pycodestyle --max-line-length=200 *.py

python3 -m unittest -v aoc18*
