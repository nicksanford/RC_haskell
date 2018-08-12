#!/usr/bin/env python3
import urllib.parse
import hashlib
import sys

def main():
    file_name = sys.argv[1]
    with open(file_name, 'rb') as f:
        file_contents = f.read()
    sha1 = hashlib.sha1(file_contents)
    print(urllib.parse.urlencode({'a': sha1.digest()}))

if __name__ == '__main__':
    main()
