import itertools

def day1a(input):
    return sum([int(x) for x in input.split(', ')])

def day1b(input):
    curr_freq = 0
    freqs_seen = set()
    for n in itertools.cycle(input.split(', ')):
        curr_freq += int(n)
        if curr_freq in freqs_seen:
            return curr_freq
        freqs_seen.add(curr_freq)
