import urllib.parse
import hashlib
import binascii

decoded = b'\x124Vx\x9a\xbc\xde\xf1#Eg\x89\xab\xcd\xef\x124Vx\x9a'
decodedhex = b'123456789abcdef123456789abcdef123456789a'
encoded = '%e4%be%9eM%b8v%e3%e3%17%97x%b0%3e%90b%97%be%5c%8d%be'
allowed = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXZYZ0123456789.-_~"

def decode(x):
    if not x:
        return ''

    if x[0] == '%':
        return x[1:3] + decode(x[3:])
    else:
        return format(ord(x[0]), '02x') + decode(x[1:])


def encode(x):
    if not x:
        return x
    next_byte = x[0:2]
    char_of_byte = chr(int(next_byte, 16))
    if char_of_byte in allowed:
        return char_of_byte + encode(x[2:])
    else:
        return "%" + next_byte + encode(x[2:])



print('decoded')
print(decoded)
print()

print('decodedhex')
print(decodedhex)
print()

print('encoded')
print(encoded)
print()

print('urllib.parse.quote(decoded) == decodedhex')
print(binascii.hexlify(decoded) == decodedhex)
print()

#binascii.hexlify(decoded) == decodedhex
#decoded = urllib.parse.unquote_to_bytes(encoded)
#decoded_downcased = urllib.parse.unquote_to_bytes(encoded_downcased)

#print('encoded')
#print(encoded)
#print()

#print('encoded_downcased')
#print(encoded_downcased)
#print()

#print("decoded (non hex)")
#print(decoded)
#print()

#print("decoded_downcased (non hex)")
#print(decoded_downcased)
#print()

#print("decoded (in hex)")
#print(decoded.hex())
#print()

#print("decoded_downcased (in hex)")
#print(decoded_downcased.hex())
#print()

#binascii.hexlify(binascii.unhexlify(a)) == a.encode()

#print("decoded (in hex, reencoded)")
#print(decoded.hex().encode('hex'))
#print()

#print("decoded_downcased (in hex, reencoded)")
#print(decoded_downcased.hex().encode('hex'))
#print()

#print("decoded (raw) reencoded")
#print(urllib.parse.quote(decoded))
#print()

#print("decoded (raw) reencoded")
#print(urllib.parse.quote(decoded_downcased))
#print()

#print("decoded, reencoded is equal to encoded")
#print(urllib.parse.quote(decoded) == encoded)
#print()

#print("decoded, reencoded, and downcased is equal to encoded")
#print(urllib.parse.quote(decoded).lower() == encoded)
#print()
#print(urllib.parse.quote(decoded).lower())


#print("decoded (hex) reencoded")
#print(urllib.parse.quote(decoded.hex()))
#print()
