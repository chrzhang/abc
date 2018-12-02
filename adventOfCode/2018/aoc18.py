#!/usr/bin/python3
"""Check that our solutions work against the inputs."""
import unittest
import solutions


class TestHelpers(unittest.TestCase):
    """Test helper functions."""
    pass


# Test against website sample inputs (c), part 1 (a), and part 2 (b).


class TestDay1(unittest.TestCase):
    def setUp(self):
        with open('inputs/day1_input', 'r') as infile:
            lines = infile.read()
        self.tidylines = ', '.join(filter(None, lines.split('\n')))

    def test_c(self):
        self.assertEqual(3, solutions.day1a('+1, -2, +3, +1'))
        self.assertEqual(3, solutions.day1a('+1, +1, +1'))
        self.assertEqual(0, solutions.day1a('+1, +1, -2'))
        self.assertEqual(-6, solutions.day1a('-1, -2, -3'))
        self.assertEqual(2, solutions.day1b('+1, -2, +3, +1'))

    def test_a(self):
        self.assertEqual(439, solutions.day1a(self.tidylines))

    def test_b(self):
        self.assertEqual(124645, solutions.day1b(self.tidylines))


class TestDay2(unittest.TestCase):
    def setUp(self):
        with open('inputs/day2_input', 'r') as infile:
            self.lines = [l.strip() for l in infile.readlines()]

    def test_c(self):
        self.assertEqual(12, solutions.day2a(['abcdef',
                                              'bababc',
                                              'abbcde',
                                              'abcccd',
                                              'aabcdd',
                                              'abcdee',
                                              'ababab']))

    def test_a(self):
        self.assertEqual(5368, solutions.day2a(self.lines))

    def test_b(self):
        self.assertEqual('cvgywxqubnuaefmsljdrpfzyi', solutions.day2b(self.lines))


if __name__ == '__main__':
    unittest.main()
