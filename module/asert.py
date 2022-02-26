#!/usr/bin/env python3
# Copyright (c) 2020 The Bitcoin developers
# Distributed under the MIT/X11 software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
#
# This program validates that a Python implementation of the ASERT algorithm
# produces identical outputs to test vectors generated from the C++
# implementation in pow.cpp.
#
# The test vectors must be in 'run*' text files in the current directory.
# These run files are produced by the gen_asert_test_vectors program
# which can be built using the Makefile in this folder (you will first
# need to do a build of BCHN itself. See the description in the Makefile.
#
# If arguments are given, they must be the names of run* files in the
# current directory.
# If no arguments are given, the list of run* files is determined by
# inspecting the current directory and all such files are processed.

import os
import sys

# Parameters needed by ASERT
IDEAL_BLOCK_TIME = 10 * 60
HALFLIFE = 2 * 24 * 3600
# Integer implementation uses these for fixed point math
RBITS = 16      # number of bits after the radix for fixed-point math
RADIX = 1 << RBITS
# POW Limit
MAX_BITS = 0x1d00ffff


def bits_to_target(bits):
    size = bits >> 24
    assert size <= 0x1d

    word = bits & 0x00ffffff
    assert 0x8000 <= word <= 0x7fffff

    if size <= 3:
        return word >> (8 * (3 - size))
    else:
        return word << (8 * (size - 3))


MAX_TARGET = bits_to_target(MAX_BITS)


def target_to_bits(target):
    assert target > 0
    if target > MAX_TARGET:
        print('Warning: target went above maximum ({} > {})'.format(target, MAX_TARGET))
        target = MAX_TARGET
    size = (target.bit_length() + 7) // 8
    mask64 = 0xffffffffffffffff
    if size <= 3:
        compact = (target & mask64) << (8 * (3 - size))
    else:
        compact = (target >> (8 * (size - 3))) & mask64

    if compact & 0x00800000:
        compact >>= 8
        size += 1

    assert compact == (compact & 0x007fffff)
    assert size < 256
    return compact | size << 24


def bits_to_work(bits):
    return (2 << 255) // (bits_to_target(bits) + 1)


def target_to_hex(target):
    h = hex(target)[2:]
    return '0' * (64 - len(h)) + h


def next_bits_aserti3_2d(anchor_bits, time_diff, height_diff):
    ''' Integer ASERTI algorithm, based on Jonathan Toomim's `next_bits_aserti`
    implementation in mining.py (see https://github.com/jtoomim/difficulty)'''

    target = bits_to_target(anchor_bits)

    # Ultimately, we want to approximate the following ASERT formula, using only integer (fixed-point) math:
    #     new_target = old_target * 2^((time_diff - IDEAL_BLOCK_TIME*(height_diff+1)) / HALFLIFE)

    # First, we'll calculate the exponent, using floor division.
    exponent = int(((time_diff - IDEAL_BLOCK_TIME * (height_diff + 1)) * RADIX) / HALFLIFE)

    # Next, we use the 2^x = 2 * 2^(x-1) identity to shift our exponent into the (0, 1] interval.
    shifts = exponent >> RBITS
    exponent -= shifts * RADIX
    assert(exponent >= 0 and exponent < 65536)

    # Now we compute an approximated target * 2^(fractional part) * 65536
    # target * 2^x ~= target * (1 + 0.695502049*x + 0.2262698*x**2 + 0.0782318*x**3)
    target *= RADIX + ((195766423245049 * exponent + 971821376 * exponent**2 + 5127 * exponent**3 + 2**47) >> (RBITS * 3))

    # Next, we shift to multiply by 2^(integer part). Python doesn't allow shifting by negative integers, so:
    if shifts < 0:
        target >>= -shifts
    else:
        target <<= shifts
    # Remove the 65536 multiplier we got earlier
    target >>= RBITS

    if target == 0:
        return target_to_bits(1)
    if target > MAX_TARGET:
        return MAX_BITS

    return target_to_bits(target)
