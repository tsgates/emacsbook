#!/usr/bin/python

import os
import sys

def ceaser(plain, shift, prime):
    def _group(c):
        return chr((ord(c) - ord("a") + shift) % prime + ord("a"))
    return "".join(map(_group, plain))

if __init__ == "__main__":
    PRIME = 17
    SHIFT = 11

    if len(sys.argv) != 2:
        print("usage: %s [text]" % sys.argv[0])
        exit(1)

    txt = sys.argv[1]
    enc = ceaser(txt,  SHIFT, PRIME)
    dec = ceaser(enc, -SHIFT, PRIME)

    print("'%s' =enc->> '%s' =dec->> '%s'\n" % (txt, enc, dec))
