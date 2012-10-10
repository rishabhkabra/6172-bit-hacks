"""
The purpose of make_tests.py is to generate tests for everybit
binary, which rotates subarrays of bits, such that the state space
for the binary is explored as exhaustively as possible.
"""

from argparse import ArgumentParser
from BitVector import BitVector
from random import getrandbits, uniform
from sys import maxint
from time import time

class _BitarrayContentType:
	# _BitarrayContentType is an enum type representing various permutations
	# of bitarray content.
	RANDOM = 1 				# e.g. 10110001 01100101
	ALL_ZEROES = 2 			# e.g. 00000000 00000000
	ALL_ONES = 3 			# e.g. 11111111 11111111
	ALTERNATING = 4 		# e.g. 01010101 01010101
	ZEROES_THEN_ONES = 5 	# e.g. 00000000 11111111
	CONTENT_TYPES = [RANDOM, ALL_ZEROES, ALL_ONES, ALTERNATING, ZEROES_THEN_ONES]
		# a list of all possible
		# enum values

class _BitarraySize:
	# _BitarraySize is an enum type representing various bitarray sizes.
	ONE = 1
	FIVE = 5
	TEN = 10
	TWENTY = 20
	THIRTY = 30
	SIXTY = 60
	HUNDRED = 100
	TWO_FIFTY = 250
	FIVE_HUNDRED = 500
	THOUSAND = 1000
	TWO_THOUSAND = 2000
	FOUR_THOUSAND = 4000
	# EIGHT_THOUSAND = 8000
	# SIXTEEN_THOUSAND = 16000
	# THIRTY_THOUSAND = 30000
	# SEVENTY_THOUSAND = 70000
	# HUNDRED_THOUSAND = 1000000
	# TWO_FIFTY_THOUSAND = 250000
	# FIVE_HUNDRED_THOUSAND = 500000
	# ONE_MILLION = 1000000
	# TWO_MILLION = 2000000
	# FOUR_MILLION = 4000000
	# EIGHT_MILLION = 8000000
	# SIXTEEN_MILLION = 16000000
	# THIRTY_MILLION = 30000000
	# SEVENTY_MILLION = 70000000
	# HUNDRED_MILLION = 100000000
	# TWO_FIFTY_MILLION = 250000000
	# FIVE_HUNDRED_MILLION = 500000000
	# ONE_BILLION = 1000000000
	# TWO_BILLION = 2000000000
	SIZES = [ONE, FIVE, TEN, TWENTY, THIRTY, SIXTY, HUNDRED, TWO_FIFTY, FIVE_HUNDRED, THOUSAND, \
		TWO_THOUSAND, FOUR_THOUSAND]#, EIGHT_THOUSAND, SIXTEEN_THOUSAND, THIRTY_THOUSAND, SEVENTY_THOUSAND, \
		# HUNDRED_THOUSAND, TWO_FIFTY_THOUSAND, FIVE_HUNDRED_THOUSAND, ONE_MILLION, TWO_MILLION, FOUR_MILLION, \
		# EIGHT_MILLION, SIXTEEN_MILLION, THIRTY_MILLION, SEVENTY_MILLION, HUNDRED_MILLION, TWO_FIFTY_MILLION, \
		# FIVE_HUNDRED_MILLION, ONE_BILLION, TWO_BILLION]
		# a list of all possible
		# enum values

class _BitOffset:
	# _BitOffset is an enum type representing various bitarray offsets.
	ZERO = 0 
	ONE = 1
	MAX = 2
	OFFSETS = [ZERO, ONE, MAX]
		# a list of all possible
		# enum values

class _BitSubarrayLength:
	# _BitSubarrayLength is an enum type representing various bit subarray sizes.
	ZERO = 0
	ONE = 1
	MAX = 2
	SUBARRAY_LENGTHS = [ZERO, ONE, MAX]
		# a list of all possible
		# enum values

class _BitRotationFactor:
	# _BitRotationFactor is an enum type representing various ways to rotate a bitarray.
	# LEFT_MAX = -maxint-1
	LEFT_ELEVENTH = -1/11.
	LEFT_SEVENTH = -1/7.
	LEFT_QUARTER = -1/4.		
	LEFT_HALF = -1/2. 		# e.g. rotate bitarray left by len(bitarray)/2 elements
	LEFT_ONE = -1 			# e.g. rotate bitarray left by 1 element
	ZERO = 0
	RIGHT_ONE = 1 			# e.g. rotate bitarray right by 1 element
	RIGHT_HALF = 1/2. 		# e.g. rotate bitarray right by len(bitarray)/2 elements
	RIGHT_QUARTER = 1/4.
	RIGHT_SEVENTH = 1/7.
	RIGHT_ELEVENTH = 1/11.
	# RIGHT_MAX = maxint
	ROTATION_FACTORS = [LEFT_ELEVENTH, LEFT_SEVENTH, LEFT_QUARTER, LEFT_HALF, LEFT_ONE, \
		ZERO, RIGHT_ONE, RIGHT_HALF, RIGHT_QUARTER, RIGHT_SEVENTH, RIGHT_ELEVENTH]

class _TestSuite:
	# _TestSuite represents test suites for the bitarray rotator.

	TEST_FORMAT_STRING = "t %s\n \nn %s\nr %s %s %s\ne %s\n\n"
		# TEST_FORMAT_STRING when filled out prints to something like:
		# t 5
		#
		# n 10010110
		# r 0 8 -1
		# 00101101

	def __init__(self):
		self.test_list = []
		self.test_counter = 0

	def add_bitarray_test(self, init_bit_string, bit_offset, bit_length, bit_right_amount, expected_result):
		self.test_list.append(self.TEST_FORMAT_STRING % (self.test_counter, init_bit_string, bit_offset, bit_length, bit_right_amount, expected_result))
		self.test_counter += 1

	def get_test_string(self):
		return "".join(self.test_list)

	def write_tests_to_file(self, filename):
		test_file = open(filename, 'w+')
		test_file.write(self.get_test_string())

def get_noisy_bitarray_size(bitarray_size):
	epsilon_factor = 45
	lower_bound = bitarray_size - bitarray_size/epsilon_factor
	upper_bound = bitarray_size + bitarray_size/epsilon_factor
	return int(uniform(lower_bound, upper_bound))

def get_random_bit_string(bit_length):
	randbits = bin(getrandbits(bit_length))[2:] # [2:] slices off the leading '0b' in bin()'s return value
	leading_zeroes = bit_length - len(randbits)
	randbits = ('0' * leading_zeroes) + randbits
	return randbits

def get_uniform_bit_string(bit, bit_length):
	return str(bit) * bit_length
	
def get_alternating_bit_string(bit_length):
	alternating_bits = ''
	while len(alternating_bits) != bit_length:
		if len(alternating_bits) % 2 == 0:
			alternating_bits += '0'
		else:
			alternating_bits += '1'
	return alternating_bits

def get_bimodal_bit_string(starting_bit, bit_length):
	bimodal_bits = ''
	ending_bit = 1 - starting_bit
	while len(bimodal_bits) != bit_length/2:
		bimodal_bits += str(starting_bit)
	while len(bimodal_bits) != bit_length:
		bimodal_bits += str(ending_bit)
	return bimodal_bits

def get_init_bit_string(content_type, bitarray_size):
	init_bit_string = ''
	if bitarray_size != 0:
		if content_type == _BitarrayContentType.RANDOM:
			init_bit_string = get_random_bit_string(bitarray_size)
		elif content_type == _BitarrayContentType.ALL_ZEROES:
			init_bit_string = get_uniform_bit_string(0, bitarray_size)
		elif content_type == _BitarrayContentType.ALL_ONES:
			init_bit_string = get_uniform_bit_string(1, bitarray_size)
		elif content_type == _BitarrayContentType.ALTERNATING:
			init_bit_string = get_alternating_bit_string(bitarray_size)
		elif content_type == _BitarrayContentType.ZEROES_THEN_ONES:
			init_bit_string = get_bimodal_bit_string(0, bitarray_size)
		elif content_type == _BitarrayContentType.ONES_THEN_ZEROES:
			init_bit_string = get_bimodal_bit_string(1, bitarray_size)
	return init_bit_string

def get_random_offset(bit_length):
	if bit_length == 0:
		return 0
	else:
		return int(round(uniform(0, bit_length - 1)))

def get_random_subbit_length(max_subbit_length):
	return int(round(uniform(0, max_subbit_length)))

def get_bit_right_amount(rotation_factor, bitarray_size):
	# if rotation_factor == _BitRotationFactor.LEFT_MAX:
	# 	return -maxint-1
	# elif rotation_factor == _BitRotationFactor.RIGHT_MAX:
	# 	return maxint
	if rotation_factor == _BitRotationFactor.LEFT_ONE:
		return -1
	elif rotation_factor == _BitRotationFactor.RIGHT_ONE:
		return 1
	else:
		return int(bitarray_size*rotation_factor)

def get_expected_result(init_bit_string, bit_offset, subbit_length, bit_right_amount):
	if subbit_length == 0 or abs(bit_right_amount) % subbit_length == 0: # the trivial cases
		return init_bit_string
	else:
		reduced_bit_right_amount = abs(bit_right_amount) % subbit_length
		if bit_right_amount < 0: # since % operation always returns nonnegative value
			reduced_bit_right_amount *= -1
		bitarray = BitVector(bitstring = init_bit_string)
		bitarray[bit_offset:(bit_offset + subbit_length)] = bitarray[bit_offset:(bit_offset + subbit_length)] >> reduced_bit_right_amount
		return str(bitarray)

def parse_args():
	parser = ArgumentParser()
	parser.add_argument("standard", help="if true, generates the standard (complete) test suite; else, generates test suite according to bitarray_lenghts/num_tests params")
	parser.add_argument("bitarray_lengths", help="comma-separated lengths of bitarrays to generate tests for")
	parser.add_argument("num_tests", help="number of tests to generate for each bitarray length specified",type=int)
	return parser.parse_args()

def create_standard_test_suite(num_random_tests_per_bitarray_length):
	test_suite = _TestSuite()
	for content_type in _BitarrayContentType.CONTENT_TYPES:
	 	for bitarray_size in _BitarraySize.SIZES:
			bitarray_size = get_noisy_bitarray_size(bitarray_size)
			init_bit_string = get_init_bit_string(content_type, bitarray_size)
			for bit_offset in _BitOffset.OFFSETS:
				if bit_offset == _BitOffset.MAX:
					bit_offset = bitarray_size - 1
				if bit_offset >= bitarray_size:
					continue
				for subarray_length in _BitSubarrayLength.SUBARRAY_LENGTHS:
					if subarray_length == _BitSubarrayLength.MAX:
						subarray_length = bitarray_size - bit_offset
					for rotation_factor in _BitRotationFactor.ROTATION_FACTORS:
						bit_right_amount = get_bit_right_amount(rotation_factor, bitarray_size)
						expected_result = get_expected_result(init_bit_string, bit_offset, subarray_length, bit_right_amount)
						test_suite.add_bitarray_test(init_bit_string, bit_offset, subarray_length, bit_right_amount, expected_result)
				for i in xrange(num_random_tests_per_bitarray_length):
					max_subarray_length = bitarray_size - bit_offset
					subarray_length = get_random_subbit_length(max_subarray_length)
					for rotation_factor in _BitRotationFactor.ROTATION_FACTORS:
						bit_right_amount = get_bit_right_amount(rotation_factor, bitarray_size)
						expected_result = get_expected_result(init_bit_string, bit_offset, subarray_length, bit_right_amount)
						test_suite.add_bitarray_test(init_bit_string, bit_offset, subarray_length, bit_right_amount, expected_result)
			for i in xrange(num_random_tests_per_bitarray_length):
				bit_offset = get_random_offset(bitarray_size)
				for subarray_length in _BitSubarrayLength.SUBARRAY_LENGTHS:
					if subarray_length == _BitSubarrayLength.MAX:
						subarray_length = bitarray_size - bit_offset
					for rotation_factor in _BitRotationFactor.ROTATION_FACTORS:
						bit_right_amount = get_bit_right_amount(rotation_factor, bitarray_size)
						expected_result = get_expected_result(init_bit_string, bit_offset, subarray_length, bit_right_amount)
						test_suite.add_bitarray_test(init_bit_string, bit_offset, subarray_length, bit_right_amount, expected_result)
				for i in xrange(num_random_tests_per_bitarray_length):
					max_subarray_length = bitarray_size - bit_offset
					subarray_length = get_random_subbit_length(max_subarray_length)
					for rotation_factor in _BitRotationFactor.ROTATION_FACTORS:
						bit_right_amount = get_bit_right_amount(rotation_factor, bitarray_size)
						expected_result = get_expected_result(init_bit_string, bit_offset, subarray_length, bit_right_amount)
						test_suite.add_bitarray_test(init_bit_string, bit_offset, subarray_length, bit_right_amount, expected_result)
	test_suite.write_tests_to_file("default")


def main():
	args = parse_args()
	if args.standard == 'T':
		num_random_tests_per_bitarray_length = args.num_tests
		create_standard_test_suite(num_random_tests_per_bitarray_length)

if __name__ == '__main__':
	main()
