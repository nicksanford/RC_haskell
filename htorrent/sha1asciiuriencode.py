#!/usr/bin/env python3
import hashlib
import binascii
import sys
import urllib.parse

x = b'%e4%be%9eM%b8v%e3%e3%17%97x%b0%3e%90b%97%be%5c%8d%be'
def main():
    file_name = sys.argv[1]
    with open(file_name, 'r', encoding='latin1') as f:
        file_contents = f.read()
    sha1 = hashlib.sha1(file_contents.encode())
    unhexified = binascii.unhexlify(sha1.hexdigest())
    print(unhexified)
    quoted = urllib.parse.quote(unhexified)
    print(quoted)
    print(repr(urllib.parse.unquote_to_bytes(quoted) == unhexified))
    print(repr(x))
    print(repr(urllib.parse.unquote_to_bytes(x)))

if __name__ == '__main__':
    main()
