#!/usr/bin/env python3
import hashlib
import binascii
import sys

def main():
    file_name = sys.argv[1]
    with open(file_name, 'r', encoding='latin1') as f:
        file_contents = f.read()
    sha1 = hashlib.sha1(file_contents.encode())
    print(repr(binascii.unhexlify(sha1.hexdigest())))

if __name__ == '__main__':
    main()
