import json
import sys


def main(directory):
    solution = json.loads(''.join(open('%s/solution.json' % directory, 'r').readlines()))
    candidate = json.loads(''.join(sys.stdin.readlines()))

    solution["bundle_ids"].sort()
    candidate["bundle_ids"].sort()

    print 'Correct!' if solution == candidate else 'Incorrect!'


if __name__ == '__main__':
    main(sys.argv[1] if len(sys.argv) > 0 else '.')