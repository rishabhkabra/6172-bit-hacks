/**
 * Copyright (c) 2012 MIT License by 6.172 Staff
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 **/

// Implements the ADT specified in bitarray.h as a packed array of bits; a bit
// array containing bit_sz bits will consume roughly bit_sz/8 bytes of
// memory.


#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

#include <sys/types.h>

#include "./bitarray.h"


// ********************************* Types **********************************

#define WORD unsigned long
#define WORD_SIZE_IN_BYTES (sizeof(WORD))
#define WORD_SIZE_IN_BITS (WORD_SIZE_IN_BYTES * 8)
#define MINUS_ONE_WORD (0xffffffffffffffff)
#define REVERSE_WORD(word) (reverse_unsigned_long(word))
#define WORD_BITMASK(index) (unsigned_long_bitmask(index))

// Concrete data type representing an array of bits.
struct bitarray {
  // The number of bits represented by this bit array.
  // Need not be divisible by 8.
  size_t bit_sz;

  // The underlying memory buffer that stores the bits in
  // packed form (8 per byte).
  WORD *buf;
};


// ******************** Prototypes for static functions *********************

// Rotates a subarray left by an arbitrary number of bits.
//
// bit_offset is the index of the start of the subarray
// bit_length is the length of the subarray, in bits
// bit_left_amount is the number of places to rotate the
//                    subarray left
//
// The subarray spans the half-open interval
// [bit_offset, bit_offset + bit_length)
// That is, the start is inclusive, but the end is exclusive.
static void bitarray_rotate_left(bitarray_t *const bitarray,
                                 const size_t bit_offset,
                                 const size_t bit_length,
                                 const size_t bit_left_amount);

// Rotates a subarray left by one bit.
//
// bit_offset is the index of the start of the subarray
// bit_length is the length of the subarray, in bits
//
// The subarray spans the half-open interval
// [bit_offset, bit_offset + bit_length)
// That is, the start is inclusive, but the end is exclusive.
static void bitarray_rotate_left_one(bitarray_t *const bitarray,
                                     const size_t bit_offset,
                                     const size_t bit_length);

// Portable modulo operation that supports negative dividends.
//
// Many programming languages define modulo in a manner incompatible with its
// widely-accepted mathematical definition.
// http://stackoverflow.com/questions/1907565/c-python-different-behaviour-of-the-modulo-operation
// provides details; in particular, C's modulo
// operator (which the standard calls a "remainder" operator) yields a result
// signed identically to the dividend e.g., -1 % 10 yields -1.
// This is obviously unacceptable for a function which returns size_t, so we
// define our own.
//
// n is the dividend and m is the divisor
//
// Returns a positive integer r = n (mod m), in the range
// 0 <= r < m.
static size_t modulo(const ssize_t n, const size_t m);

// Produces a mask which, when ANDed with a byte, retains only the
// bit_index th byte.
//
// Example: bitmask(5) produces the byte 0b00100000.
//
// (Note that here the index is counted from right
// to left, which is different from how we represent bitarrays in the
// tests.  This function is only used by bitarray_get and bitarray_set,
// however, so as long as you always use bitarray_get and bitarray_set
// to access bits in your bitarray, this reverse representation should
// not matter.


// ******************************* Functions ********************************

bitarray_t *bitarray_new(const size_t bit_sz) {
  // Allocate an underlying buffer of ceil(bit_sz/8) bytes.
  WORD *const buf = calloc(WORD_SIZE_IN_BYTES, bit_sz / WORD_SIZE_IN_BITS + ((bit_sz % WORD_SIZE_IN_BITS == 0) ? 0 : 1));
  if (buf == NULL) {
    return NULL;
  }

  // Allocate space for the struct.
  bitarray_t *const bitarray = malloc(sizeof(struct bitarray));
  if (bitarray == NULL) {
    free(buf);
    return NULL;
  }

  bitarray->buf = buf;
  bitarray->bit_sz = bit_sz;
  return bitarray;
}

void bitarray_free(bitarray_t *const bitarray) {
  if (bitarray == NULL) {
    return;
  }
  free(bitarray->buf);
  bitarray->buf = NULL;
  free(bitarray);
}


inline static unsigned long unsigned_long_bitmask(const size_t bit_index) {
  return ((unsigned long) 1) << (63 - (bit_index % WORD_SIZE_IN_BITS));
}

size_t bitarray_get_bit_sz(const bitarray_t *const bitarray) {
  return bitarray->bit_sz;
}

// TODO: inline this
bool bitarray_get(const bitarray_t *const bitarray, const size_t bit_index) {
  assert(bit_index < bitarray->bit_sz);

  // We're storing bits in packed form, 8 per byte.  So to get the nth
  // bit, we want to look at the (n mod 8)th bit of the (floor(n/8)th)
  // byte.
  //
  // In C, integer division is floored explicitly, so we can just do it to
  // get the byte; we then bitwise-and the byte with an appropriate mask
  // to produce either a zero byte (if the bit was 0) or a nonzero byte
  // (if it wasn't).  Finally, we convert that to a boolean.

  return (bitarray->buf[bit_index / WORD_SIZE_IN_BITS] & WORD_BITMASK(bit_index)) ?
             true : false;
}

// TODO: inline this
void bitarray_set(bitarray_t *const bitarray,
                  const size_t bit_index,
                  const bool value) {
  assert(bit_index < bitarray->bit_sz);

  // We're storing bits in packed form, 8 per byte.  So to set the nth
  // bit, we want to set the (n mod 8)th bit of the (floor(n/8)th) byte.
  //
  // In C, integer division is floored explicitly, so we can just do it to
  // get the byte; we then bitwise-and the byte with an appropriate mask
  // to clear out the bit we're about to set.  We bitwise-or the result
  // with a byte that has either a 1 or a 0 in the correct place.
  WORD mask = WORD_BITMASK(bit_index);
  size_t bufindex = bit_index / WORD_SIZE_IN_BITS;
  bitarray->buf[bufindex] = 
      (bitarray->buf[bufindex] & ~mask) |
      (value ? mask : 0);
}

static void bitarray_reverse(bitarray_t * bitarray, size_t bit_offset, const size_t bit_length) {
  // Reverses the array of length bitarray_length beginning
  // at bitarray_start. Assumes bitarray_start + bitarray_length - 1
  // is the address to the last of element of the to-be-reversed array.

  assert(bit_offset + bit_length <= bitarray->bit_sz);

  if (bit_length <= 1) {
    return;
  }
 
  size_t end_index = bit_offset + bit_length - 1; 
  bool temp;
  while(bit_offset < end_index) {
    temp = bitarray_get(bitarray, bit_offset);
    bitarray_set(bitarray, bit_offset, bitarray_get(bitarray, end_index));
    bitarray_set(bitarray, end_index, temp);
    bit_offset++;
    end_index--;
  }
}

static const unsigned long char_reverse_lookup_table[256] = 
{
#define R2(n) n, n+2*64, n+1*64, n+3*64
#define R4(n) R2(n), R2(n+2*16), R2(n+1*16), R2(n+3*16)
#define R6(n) R4(n), R4(n+2*4), R4(n+1*4), R4(n+3*4)
  R6(0), R6(2), R6(1), R6(3)
}; // reference: Bit Twiddling Hacks, by Sean Eron Anderson (seander@cs.stanford.edu). Table definition suggested by Hallvard Furuseth on July 14, 2009.

/*
static char reverse_char(char c) {
  return char_reverse_lookup_table[c]; 
}

static unsigned int reverse_unsigned_int(unsigned int i) {
  return (char_reverse_lookup_table[i & 0xff] << 24) |
      (char_reverse_lookup_table[(i >> 8) & 0xff] << 16) |
      (char_reverse_lookup_table[(i >> 16) & 0xff] << 8) |
      (char_reverse_lookup_table[(i >> 24) & 0xff]);
}
*/

static unsigned long reverse_unsigned_long(unsigned long l) {
  return (char_reverse_lookup_table[l & 0xff] << 56) |
      (char_reverse_lookup_table[(l >> 8) & 0xff] << 48) |
      (char_reverse_lookup_table[(l >> 16) & 0xff] << 40) |
      (char_reverse_lookup_table[(l >> 24) & 0xff] << 32) |
      (char_reverse_lookup_table[(l >> 32) & 0xff] << 24) |
      (char_reverse_lookup_table[(l >> 40) & 0xff] << 16) |
      (char_reverse_lookup_table[(l >> 48) & 0xff] << 8) |
      (char_reverse_lookup_table[(l >> 56) & 0xff]);
}

/*
//Slower implementation for reversing unsigned long values:
static unsigned long reverse_unsigned_long2 (unsigned long l) {
  l = ((l >> 1) & 0x5555555555555555) | ((l & 0x5555555555555555) << 1); // swap odd and even bits
  l = ((l >> 2) & 0x3333333333333333) | ((l & 0x3333333333333333) << 2); // swap consecutive pairs
  l = ((l >> 4) & 0x0F0F0F0F0F0F0F0F) | ((l & 0x0F0F0F0F0F0F0F0F) << 4); // swap nibbles
  l = ((l >> 8) & 0x00FF00FF00FF00FF) | ((l & 0x00FF00FF00FF00FF) << 8); // swap bytes
  l = ((l >> 16) & 0x0000FFFF0000FFFF) | ((l & 0x0000FFFF0000FFFF) << 16); // swap consecutive byte-pairs
  l = (l >> 32) | (l << 32); // swap 32-bit words
  return l;
}
*/

static void bitarray_reverse_on_steroids(bitarray_t * bitarray, size_t bit_offset, const size_t bit_length) {

  assert(bit_offset + bit_length <= bitarray->bit_sz);

  if (bit_length <= WORD_SIZE_IN_BITS * 2) {
    bitarray_reverse(bitarray, bit_offset, bit_length);
    return;
  }

  size_t leftexcess = WORD_SIZE_IN_BITS - (bit_offset % WORD_SIZE_IN_BITS);
  WORD * leftword = bitarray->buf + bit_offset / WORD_SIZE_IN_BITS;
  size_t rightexcess = (bit_offset + bit_length) % WORD_SIZE_IN_BITS;
  if (rightexcess == 0) {
    rightexcess = WORD_SIZE_IN_BITS;
  }
  WORD * rightword = bitarray->buf + (bit_offset + bit_length - 1) / WORD_SIZE_IN_BITS;

  WORD x, y, mask1, mask2, temp;
  while (leftword < rightword) {
  if (leftexcess < rightexcess) {
    mask1 = MINUS_ONE_WORD << (WORD_SIZE_IN_BITS - leftexcess); // desired mask is a char with exactly #leftexcess ones followed by zeroes. used to preserve selected bits and destroy rest.
    mask2 = MINUS_ONE_WORD >> (WORD_SIZE_IN_BITS - leftexcess); // desired mask is a char that ends with exactly #leftexcess zeroes. all preceding bits are ones. used to destroy certain bits and preserve rest.
    x = (REVERSE_WORD(*leftword) & mask1) >> (rightexcess - leftexcess);
    y = (REVERSE_WORD(*rightword) & (mask1 >> (WORD_SIZE_IN_BITS - rightexcess))) >> (rightexcess - leftexcess);
    *leftword = (*leftword & ~mask2) | y;
    *rightword = (*rightword & ~(mask2 << (WORD_SIZE_IN_BITS - rightexcess))) | x;

    rightexcess -= leftexcess;
    leftword++;
    leftexcess = WORD_SIZE_IN_BITS;

  } else if (leftexcess > rightexcess) {
    mask1 = MINUS_ONE_WORD >> (WORD_SIZE_IN_BITS - rightexcess); // desired mask is a char that ends with exactly #rightexcess ones. all preceding bits are zeroes. used to preserve selected bits and destroy rest.
    mask2 = MINUS_ONE_WORD << (WORD_SIZE_IN_BITS - rightexcess); // desired mask is a char with exactly #rightexcess ones followed by zeroes. Its complement is used to destroy selected bits and preserve rest.
    x = (REVERSE_WORD(*leftword) & (mask1 << (WORD_SIZE_IN_BITS - leftexcess))) << (leftexcess - rightexcess);
    y = (REVERSE_WORD(*rightword) & mask1) << (leftexcess - rightexcess);
    *leftword = (*leftword & ~(mask2 >> (WORD_SIZE_IN_BITS - leftexcess))) | y;
    *rightword = (*rightword & ~mask2) | x;

    leftexcess -= rightexcess;
    rightword--;
    rightexcess = WORD_SIZE_IN_BITS;

  } else {  // case 3: leftexcess == rightexcess
    if (leftexcess != 0) {
      mask1 = MINUS_ONE_WORD >> (WORD_SIZE_IN_BITS - leftexcess); // desired mask is a char that ends with exactly #leftexcess == #rightexcess ones. all preceding bits are zeroes. used to preserve selected bits and destroy others.
      mask2 = ~mask1; 
      x = REVERSE_WORD(*leftword & mask1);
      y = REVERSE_WORD(*rightword) & mask1;
      *leftword = (*leftword & mask2) | y;
      *rightword = (*rightword & REVERSE_WORD(mask2)) | x;
    } else {
      temp = REVERSE_WORD(*leftword);
      *leftword = REVERSE_WORD(*rightword);
      *rightword = temp;
    }
    leftword++;
    leftexcess = WORD_SIZE_IN_BITS;
    rightword--;
    rightexcess = WORD_SIZE_IN_BITS;
  }
  }

  if (leftword == rightword) {
    size_t bit_offset2, bit_length2;
    if (leftexcess == WORD_SIZE_IN_BITS) {
      bit_offset2 = (leftword - (bitarray->buf)) * WORD_SIZE_IN_BITS;
      bit_length2 = rightexcess;
    } else {
      bit_offset2 = ((leftword - (bitarray->buf) + 1) * WORD_SIZE_IN_BITS) - leftexcess;
      bit_length2 = leftexcess;
    }
    bitarray_reverse(bitarray, bit_offset2, bit_length2);}
}


void bitarray_rotate(bitarray_t *const bitarray, const size_t bit_offset, const size_t bit_length, const ssize_t bit_right_amount) {
  // implements bitarray rotation via clever (A^R + B^R)^R 
  // algorithm discussed in project 1 handout
  assert(bit_offset + bit_length <= bitarray->bit_sz);
  
  //printf("bitarray_rotate() is being called.\n");
  //printf("bitarray: %p\n", bitarray);
  //printf("bit_offset: %d\n",(int) bit_offset);
  //printf("bit_length: %d\n", (int) bit_length);
  //printf("bit_right_amount: %d\n", (int) bit_right_amount); 
  
  if (bit_length == 0) {
    return;
  }

  const size_t bit_left_amount = modulo(-bit_right_amount, bit_length);
  
  //printf("Rotating A...\n"); 
  bitarray_reverse_on_steroids(bitarray, bit_offset, bit_left_amount);

  //printf("Rotating B...\n");
  bitarray_reverse_on_steroids(bitarray, bit_offset + bit_left_amount, bit_length - bit_left_amount);
  
  //printf("Rotating A+B...\n");
  bitarray_reverse_on_steroids(bitarray, bit_offset, bit_length);
  
  //printf("Concluded rotation.\n");
}

void bitarray_rotate_2(bitarray_t *const bitarray,
                     const size_t bit_offset,
                     const size_t bit_length,
                     const ssize_t bit_right_amount) {
  // original bitarray rotation implementation
  assert(bit_offset + bit_length <= bitarray->bit_sz);

  if (bit_length == 0) {
    return;
  }

  // Convert a rotate left or right to a left rotate only, and eliminate
  // multiple full rotations.
  bitarray_rotate_left(bitarray, bit_offset, bit_length,
           modulo(-bit_right_amount, bit_length));
}

static void bitarray_rotate_left(bitarray_t *const bitarray,
                                 const size_t bit_offset,
                                 const size_t bit_length,
                                 const size_t bit_left_amount) {
  for (size_t i = 0; i < bit_left_amount; i++) {
    bitarray_rotate_left_one(bitarray, bit_offset, bit_length);
  }
}

static void bitarray_rotate_left_one(bitarray_t *const bitarray,
                                     const size_t bit_offset,
                                     const size_t bit_length) {
  // Grab the first bit in the range, shift everything left by one, and
  // then stick the first bit at the end.
  const bool first_bit = bitarray_get(bitarray, bit_offset);
  size_t i;
  for (i = bit_offset; i + 1 < bit_offset + bit_length; i++) {
    bitarray_set(bitarray, i, bitarray_get(bitarray, i + 1));
  }
  bitarray_set(bitarray, i, first_bit);
}

static size_t modulo(const ssize_t n, const size_t m) {
  const ssize_t signed_m = (ssize_t)m;
  assert(signed_m > 0);
  const ssize_t result = ((n % signed_m) + signed_m) % signed_m;
  assert(result >= 0);
  return (size_t)result;
}
