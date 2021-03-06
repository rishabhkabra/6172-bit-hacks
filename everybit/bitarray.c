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

#define WORD unsigned long // defines the word type in which our bits will be stored
#define WORD_SIZE_IN_BYTES (sizeof(WORD))
#define WORD_SIZE_IN_BITS (WORD_SIZE_IN_BYTES * 8)
#define POSITIVE_ONE_WORD (1ULL)
#define REVERSE_WORD(word) (reverse_unsigned_long(word))

#define BEGINNING_ONES_BITMASK(i) (unsigned_long_bitmask_with_beginning_ones_lookup_table[i]) //the desired bitmask is a WORD that begins with exactly i ones; the remaining bits are all zeroes.
#define TRAILING_ONES_BITMASK(i) (unsigned_long_bitmask_with_trailing_ones_lookup_table[i]) //the desired bitmask is a WORD that ends with exactly i ones; the preceding bits are all zeroes.

#define REVERSE_FASTER_COARSENESS 2

// Concrete data type representing an array of bits.
struct bitarray {
  // The number of bits represented by this bit array.
  // Need not be divisible by 8.
  size_t bit_sz;

  // The underlying memory buffer that stores the bits in
  // packed form.
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
// Rotates a subarray left by one bit.
//
// bit_offset is the index of the start of the subarray
// bit_length is the length of the subarray, in bits
//
// The subarray spans the half-open interval
// [bit_offset, bit_offset + bit_length)
// That is, the start is inclusive, but the end is exclusive.
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


// ******************************* Functions ********************************

bitarray_t *bitarray_new(const size_t bit_sz) {
  // Allocate an underlying buffer of ceil(bit_sz/WORD_SIZE_IN_BITS) bytes.
  WORD *const buf = (WORD *) calloc(WORD_SIZE_IN_BYTES, bit_sz / WORD_SIZE_IN_BITS + ((bit_sz % WORD_SIZE_IN_BITS == 0) ? 0 : 1));
  if (buf == NULL) {
    return NULL;
  }

  // Allocate space for the struct.
  bitarray_t *const bitarray = (bitarray_t *) malloc(sizeof(struct bitarray));
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


inline static WORD single_one_bitmask(const size_t bit_index) {
// A helper function for bitarray_get and bitarray_set that returns
// a WORD containing a single one. The returned bitmask is then used
// to set or get an individual bit.
  return (POSITIVE_ONE_WORD) << (WORD_SIZE_IN_BITS - 1 - (bit_index % WORD_SIZE_IN_BITS));
}

size_t bitarray_get_bit_sz(const bitarray_t *const bitarray) {
  return bitarray->bit_sz;
}

inline bool bitarray_get(const bitarray_t *const bitarray, const size_t bit_index) {
  assert(bit_index < bitarray->bit_sz);

  // We're storing bits in packed form (in words). So to get the nth
  // bit, we want to look at the (n mod WORD_SIZE_IN_BITS)th bit of the 
  // (floor(n/WORD_SIZE_IN_BITS)th) word.
  //
  // In C, integer division is floored explicitly, so we can just do it to
  // get the byte; we then bitwise-and the byte with an appropriate mask
  // to produce either a zero byte (if the bit was 0) or a nonzero byte
  // (if it wasn't).  Finally, we convert that to a boolean.

  return (bitarray->buf[bit_index / WORD_SIZE_IN_BITS] & single_one_bitmask(bit_index)) ?
             true : false;
}

inline void bitarray_set(bitarray_t *const bitarray,
                  const size_t bit_index,
                  const bool value) {
  assert(bit_index < bitarray->bit_sz);

  // We're storing bits in packed form (in words). So to set the nth
  // bit, we want to look at the (n mod WORD_SIZE_IN_BITS)th bit of the 
  // (floor(n/WORD_SIZE_IN_BITS)th) word.
  //
  // In C, integer division is floored explicitly, so we can just do it to
  // get the byte; we then bitwise-and the byte with an appropriate mask
  // to clear out the bit we're about to set.  We bitwise-or the result
  // with a byte that has either a 1 or a 0 in the correct place.
  WORD mask = single_one_bitmask(bit_index);
  size_t bufindex = bit_index / WORD_SIZE_IN_BITS;
  bitarray->buf[bufindex] = 
      (bitarray->buf[bufindex] & ~mask) |
      (value ? mask : 0);
}

static void bitarray_reverse(bitarray_t * bitarray, size_t bit_offset, const size_t bit_length) {
  // Reverses the array of length bitarray_length beginning
  // at bitarray_start. Assumes bitarray_start + bitarray_length - 1
  // is the address to the last of element of the to-be-reversed array.
  // This is a simplistic reversal algorithm that serves as a fallback
  // option.
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
  // Stores the reversed form of each possible byte.
  // Unsigned longs are used only as a matter of convenience.
  // Only the last 8 bits in each unsigned long are
  // occupied.
#define R2(n) n, n+2*64, n+1*64, n+3*64
#define R4(n) R2(n), R2(n+2*16), R2(n+1*16), R2(n+3*16)
#define R6(n) R4(n), R4(n+2*4), R4(n+1*4), R4(n+3*4)
  R6(0), R6(2), R6(1), R6(3)
}; // reference: Bit Twiddling Hacks, by Sean Eron Anderson (seander@cs.stanford.edu). Table definition suggested by Hallvard Furuseth.


static unsigned long reverse_unsigned_long(unsigned long l) {
  // Reverses an entire unsigned long by dividing it
  // into bytes (ie, chars) and then using the lookup
  // table defined above to identify each byte's reverse.
  unsigned char * p = (unsigned char *) &l;
  return (char_reverse_lookup_table[p[7]]) | //p[7] gives the 8 most significant 8 bits of l (little endian)
      (char_reverse_lookup_table[p[6]] << 8) |
      (char_reverse_lookup_table[p[5]] << 16) |
      (char_reverse_lookup_table[p[4]] << 24) |
      (char_reverse_lookup_table[p[3]] << 32) |
      (char_reverse_lookup_table[p[2]] << 40) |
      (char_reverse_lookup_table[p[1]] << 48) |
      (char_reverse_lookup_table[p[0]] << 56); //p[0] gives the 8 least significant bits of l
} // reference: Bit Twiddling Hacks, by Sean Eron Anderson (seander@cs.stanford.edu).

static const unsigned long unsigned_long_bitmask_with_beginning_ones_lookup_table[65] = 
{
  // Lookup table for bitmasks with [0...64] beginning ones.
  0x0, 
  0x8000000000000000, 0xc000000000000000, 0xe000000000000000, 0xf000000000000000, 
  0xf800000000000000, 0xfc00000000000000, 0xfe00000000000000, 0xff00000000000000, 
  0xff80000000000000, 0xffc0000000000000, 0xffe0000000000000, 0xfff0000000000000, 
  0xfff8000000000000, 0xfffc000000000000, 0xfffe000000000000, 0xffff000000000000, 
  0xffff800000000000, 0xffffc00000000000, 0xffffe00000000000, 0xfffff00000000000, 
  0xfffff80000000000, 0xfffffc0000000000, 0xfffffe0000000000, 0xffffff0000000000, 
  0xffffff8000000000, 0xffffffc000000000, 0xffffffe000000000, 0xfffffff000000000, 
  0xfffffff800000000, 0xfffffffc00000000, 0xfffffffe00000000, 0xffffffff00000000, 
  0xffffffff80000000, 0xffffffffc0000000, 0xffffffffe0000000, 0xfffffffff0000000, 
  0xfffffffff8000000, 0xfffffffffc000000, 0xfffffffffe000000, 0xffffffffff000000, 
  0xffffffffff800000, 0xffffffffffc00000, 0xffffffffffe00000, 0xfffffffffff00000, 
  0xfffffffffff80000, 0xfffffffffffc0000, 0xfffffffffffe0000, 0xffffffffffff0000, 
  0xffffffffffff8000, 0xffffffffffffc000, 0xffffffffffffe000, 0xfffffffffffff000, 
  0xfffffffffffff800, 0xfffffffffffffc00, 0xfffffffffffffe00, 0xffffffffffffff00, 
  0xffffffffffffff80, 0xffffffffffffffc0, 0xffffffffffffffe0, 0xfffffffffffffff0, 
  0xfffffffffffffff8, 0xfffffffffffffffc, 0xfffffffffffffffe, 0xffffffffffffffff, 
};

static const unsigned long unsigned_long_bitmask_with_trailing_ones_lookup_table[65] = 
{
  // Lookup table for bitmasks with [0...64] trailing ones.
  0x0,
  0x1, 0x3, 0x7, 0xf, 
  0x1f, 0x3f, 0x7f, 0xff, 
  0x1ff, 0x3ff, 0x7ff, 0xfff, 
  0x1fff, 0x3fff, 0x7fff, 0xffff, 
  0x1ffff, 0x3ffff, 0x7ffff, 0xfffff, 
  0x1fffff, 0x3fffff, 0x7fffff, 0xffffff, 
  0x1ffffff, 0x3ffffff, 0x7ffffff, 0xfffffff, 
  0x1fffffff, 0x3fffffff, 0x7fffffff, 0xffffffff, 
  0x1ffffffff, 0x3ffffffff, 0x7ffffffff, 0xfffffffff, 
  0x1fffffffff, 0x3fffffffff, 0x7fffffffff, 0xffffffffff, 
  0x1ffffffffff, 0x3ffffffffff, 0x7ffffffffff, 0xfffffffffff, 
  0x1fffffffffff, 0x3fffffffffff, 0x7fffffffffff, 0xffffffffffff, 
  0x1ffffffffffff, 0x3ffffffffffff, 0x7ffffffffffff, 0xfffffffffffff, 
  0x1fffffffffffff, 0x3fffffffffffff, 0x7fffffffffffff, 0xffffffffffffff, 
  0x1ffffffffffffff, 0x3ffffffffffffff, 0x7ffffffffffffff, 0xfffffffffffffff, 
  0x1fffffffffffffff, 0x3fffffffffffffff, 0x7fffffffffffffff, 0xffffffffffffffff, 
};


static void bitarray_reverse_faster(bitarray_t * bitarray, size_t bit_offset, const size_t bit_length) {
  // A faster reversal algorithm that exploits the word-level
  // representation of the bitarray. It reverses individual
  // words at either end of the subarray, exchanges their
  // positions, and proceeds inward in that manner.

  assert(bit_offset + bit_length <= bitarray->bit_sz);

  if (bit_length <= WORD_SIZE_IN_BITS * REVERSE_FASTER_COARSENESS) {
    bitarray_reverse(bitarray, bit_offset, bit_length);
    return;
  }

  size_t leftexcess = WORD_SIZE_IN_BITS - (bit_offset % WORD_SIZE_IN_BITS); // range: [1,64]
  // leftexcess is a measure of the number of bits in the leftmost word that need to be reversed and repositioned.
  WORD * leftword = bitarray->buf + bit_offset / WORD_SIZE_IN_BITS; // leftmost word of the subarray
  size_t rightexcess = (bit_offset + bit_length) % WORD_SIZE_IN_BITS; // desired range: [1:64]
  // rightexcess is a measure of the number of bits in the rightmost word that need to be reversed and repositioned.
  if (rightexcess == 0) {
    rightexcess = WORD_SIZE_IN_BITS;
  }
  WORD * rightword = bitarray->buf + (bit_offset + bit_length - 1) / WORD_SIZE_IN_BITS; // rightmost word of the subarray
  
  if (leftexcess < rightexcess) { //Case 1: subarray to be reversed is asymmetric and right-excessive
    WORD rightreplacement, left1replacement, left2replacement;
    rightreplacement = (*leftword & TRAILING_ONES_BITMASK(leftexcess)) << (rightexcess - leftexcess);
    rightreplacement |= (*(leftword+1) & BEGINNING_ONES_BITMASK(rightexcess - leftexcess)) >> (WORD_SIZE_IN_BITS - (rightexcess - leftexcess));
    rightreplacement = REVERSE_WORD(rightreplacement);

    left1replacement = REVERSE_WORD(*rightword & BEGINNING_ONES_BITMASK(rightexcess));
    left2replacement = left1replacement << (WORD_SIZE_IN_BITS - (rightexcess - leftexcess));
    left1replacement = left1replacement >> (rightexcess - leftexcess);

    *leftword = (*leftword & ~(TRAILING_ONES_BITMASK(leftexcess))) | left1replacement;
    leftword++;
    *leftword = (*leftword & ~(BEGINNING_ONES_BITMASK(rightexcess - leftexcess))) | left2replacement;
    leftexcess = WORD_SIZE_IN_BITS - (rightexcess - leftexcess);
    *rightword = (*rightword & ~(BEGINNING_ONES_BITMASK(rightexcess))) | rightreplacement;
    rightword--;
    rightexcess = WORD_SIZE_IN_BITS;
    while (rightword - leftword > 1) { // condition ensures rightword and leftword are not adjacent
    rightreplacement = (*leftword & TRAILING_ONES_BITMASK(leftexcess)) << (rightexcess - leftexcess);
    rightreplacement |= (*(leftword+1) & BEGINNING_ONES_BITMASK(rightexcess - leftexcess)) >> (leftexcess);
    rightreplacement = REVERSE_WORD(rightreplacement);

    left1replacement = REVERSE_WORD(*rightword & BEGINNING_ONES_BITMASK(rightexcess));
    left2replacement = left1replacement << (leftexcess);
    left1replacement = left1replacement >> (rightexcess - leftexcess);

    *leftword = (*leftword & ~(TRAILING_ONES_BITMASK(leftexcess))) | left1replacement;
    leftword++;
    *leftword = (*leftword & ~(BEGINNING_ONES_BITMASK(rightexcess - leftexcess))) | left2replacement;
    *rightword = (*rightword & ~(BEGINNING_ONES_BITMASK(rightexcess))) | rightreplacement;
    rightword--;
    }

  } else if (leftexcess > rightexcess) { //Case 2: subarray to be reversed is asymmetric and left-excessive
    WORD leftreplacement, right1replacement, right2replacement;
      leftreplacement = (*rightword & BEGINNING_ONES_BITMASK(rightexcess)) >> (leftexcess - rightexcess);
      leftreplacement |= (*(rightword-1) & TRAILING_ONES_BITMASK(leftexcess - rightexcess)) << (WORD_SIZE_IN_BITS - (leftexcess - rightexcess));
      leftreplacement = REVERSE_WORD(leftreplacement);

      right1replacement = REVERSE_WORD(*leftword & TRAILING_ONES_BITMASK(leftexcess));
      right2replacement = right1replacement >> (WORD_SIZE_IN_BITS - (leftexcess - rightexcess));
      right1replacement = right1replacement << (leftexcess - rightexcess);

      *rightword = (*rightword & ~(BEGINNING_ONES_BITMASK(rightexcess))) | right1replacement;
      rightword--;
      *rightword = (*rightword & ~(TRAILING_ONES_BITMASK(leftexcess - rightexcess))) | right2replacement;
      rightexcess = WORD_SIZE_IN_BITS - (leftexcess - rightexcess);
      *leftword = (*leftword & ~(TRAILING_ONES_BITMASK(leftexcess))) | leftreplacement;
      leftword++;
      leftexcess = WORD_SIZE_IN_BITS;
      while(rightword - leftword > 1) {
        leftreplacement = (*rightword & BEGINNING_ONES_BITMASK(rightexcess)) >> (leftexcess - rightexcess);
        leftreplacement |= (*(rightword-1) & TRAILING_ONES_BITMASK(leftexcess - rightexcess)) << (rightexcess);
        leftreplacement = REVERSE_WORD(leftreplacement);

        right1replacement = REVERSE_WORD(*leftword & TRAILING_ONES_BITMASK(leftexcess));
        right2replacement = right1replacement >> (rightexcess);
        right1replacement = right1replacement << (leftexcess - rightexcess);

        *rightword = (*rightword & ~(BEGINNING_ONES_BITMASK(rightexcess))) | right1replacement;
        rightword--;
        *rightword = (*rightword & ~(TRAILING_ONES_BITMASK(leftexcess - rightexcess))) | right2replacement;
        *leftword = (*leftword & ~(TRAILING_ONES_BITMASK(leftexcess))) | leftreplacement;
        leftword++;
    }

  } else { // Case 3: leftexcess == righexcess. Subarray to be reversed is symmetric.
      WORD leftreplacement, rightreplacement;
      WORD temp = TRAILING_ONES_BITMASK(leftexcess);
      rightreplacement = REVERSE_WORD(*leftword & temp);
      leftreplacement = REVERSE_WORD(*rightword) & temp;

      *leftword = (*leftword & ~temp) | leftreplacement;
      *rightword = (*rightword & ~(BEGINNING_ONES_BITMASK(leftexcess))) | rightreplacement;
      leftword++;
      rightword--;
      while (leftword < rightword) {
        temp = REVERSE_WORD(*leftword);
        *leftword = REVERSE_WORD(*rightword);
        *rightword = temp;
        leftword++;
        rightword--;
      }
      leftexcess = WORD_SIZE_IN_BITS;
      rightexcess = WORD_SIZE_IN_BITS;
  }
  
  if (leftword <= rightword) {
    size_t bit_offset2, bit_length2;
    if (leftexcess == WORD_SIZE_IN_BITS) {
      bit_offset2 = (leftword - (bitarray->buf)) * WORD_SIZE_IN_BITS;
      bit_length2 = rightexcess + ((rightword - leftword > 0) ? WORD_SIZE_IN_BITS : 0);
    } else {
      bit_offset2 = ((leftword - (bitarray->buf) + 1) * WORD_SIZE_IN_BITS) - leftexcess;
      bit_length2 = leftexcess + ((rightword - leftword > 0) ? WORD_SIZE_IN_BITS : 0);;
    }
    bitarray_reverse(bitarray, bit_offset2, bit_length2);}
}

void bitarray_rotate(bitarray_t *const bitarray, const size_t bit_offset, const size_t bit_length, const ssize_t bit_right_amount) {
  // Implements bitarray rotation via reversal algorithm:
  //   Given a bitarray B to be rotated left by bit_left_amount,
  //   let B be the left-to-right concatenation, L + R, of 
  //   a bit_left_amount-size subarray L and a (bitarray_length - bit_left_amount)-size
  //   subarray R. 
  //   Then rotate(B) by bit_left_amount = reverse(reverse(L) + reverse(R))
  assert(bit_offset + bit_length <= bitarray->bit_sz);
 
  if (bit_length == 0) {
    return;
  }

  const size_t bit_left_amount = modulo(-bit_right_amount, bit_length);
  if (bit_left_amount == 0){
    return;
  }

  bitarray_reverse_faster(bitarray, bit_offset, bit_left_amount); // reverse(L)
  bitarray_reverse_faster(bitarray, bit_offset + bit_left_amount, bit_length - bit_left_amount); // reverse(R)
  bitarray_reverse_faster(bitarray, bit_offset, bit_length); // reverse(reverse(L) + reverse(R))
}  

static size_t modulo(const ssize_t n, const size_t m) {
  const ssize_t signed_m = (ssize_t)m;
  assert(signed_m > 0);
  const ssize_t result = ((n % signed_m) + signed_m) % signed_m;
  assert(result >= 0);
  return (size_t)result;
}
