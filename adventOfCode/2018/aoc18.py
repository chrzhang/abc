#!/usr/bin/python3
"""Check that our solutions work against the inputs."""
import unittest
from solutions import *

class TestHelpers(unittest.TestCase):
    """Test helper functions."""
    pass

class TestDay1(unittest.TestCase):
    """Test against website sample inputs (c), part 1 (a), and part 2(b)."""
    def setUp(self):
        with open('inputs/day1a_input', 'r') as f:
            lines = f.read()
        self.tidylines = ', '.join(filter(None, lines.split('\n')))
    def test_day1c(self):
        self.assertEqual(3, day1a('+1, -2, +3, +1'))
        self.assertEqual(3, day1a('+1, +1, +1'))
        self.assertEqual(0, day1a('+1, +1, -2'))
        self.assertEqual(-6, day1a('-1, -2, -3'))
        self.assertEqual(2, day1b('+1, -2, +3, +1'))
    def test_day1a(self):
        self.assertEqual(439, day1a(self.tidylines))
    def test_day1b(self):
        self.assertEqual(124645, day1b(self.tidylines))

if __name__ == '__main__':
    unittest.main()
