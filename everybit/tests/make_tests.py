from argparse import ArgumentParser
from BitVector import BitVector
from random import getrandbits, uniform
import time

TEST_LIST = []
TEST_COUNTER = 0
TEST_FORMAT_STRING = "t %s\n \nn %s\nr %s %s %s\ne %s\n\n"
	## TEST_FORMAT_STRING when filled out prints to something like:
	## t 5
	##
	## n 10010110
	## r 0 8 -1
	## 00101101
NUM_TESTS_PER_BIT_LENGTH = None
ROTATION_RANGE = 50

def add_bitarray_test(init_bit_string, bit_offset, bit_length, bit_right_amount, expected_result):
	global TEST_LIST
	global TEST_FORMAT_STRING
	global TEST_COUNTER
	TEST_LIST.append(TEST_FORMAT_STRING % (TEST_COUNTER, init_bit_string, bit_offset, bit_length, bit_right_amount, expected_result))
	TEST_COUNTER += 1	

def get_init_bit_string(bit_length):
	randbits = ''
	if bit_length != 0:
		randbits = bin(getrandbits(bit_length))[2:] # [2:] slices off the leading '0b' in bin()'s return value
		while len(randbits) != bit_length:	# we'll need to pad randbits since bin() truncates leading 0's
			randbits = '0' + randbits
	return randbits

def get_random_offset(bit_length):
	if bit_length == 0:
		return 0
	else:
		return int(round(uniform(0, bit_length - 1)))

def get_random_subbit_length(max_subbit_length):
	return int(round(uniform(0, max_subbit_length)))

def get_random_right_amount():
	return int(round(uniform(-1*ROTATION_RANGE, ROTATION_RANGE)))

def get_expected_result(init_bit_string, bit_offset, subbit_length, bit_right_amount):
	if subbit_length == 0 or abs(bit_right_amount) % subbit_length == 0: # the trivial cases
		return init_bit_string
	else:
		#reduced_bit_right_amount = abs(bit_right_amount) % subbit_length
		#if bit_right_amount < 0: # since % operation always returns nonnegative value
		#	reduced_bit_right_amount *= -1
		bitarray = BitVector(bitstring = init_bit_string)
		bitarray[bit_offset:(bit_offset + subbit_length)] = bitarray[bit_offset:(bit_offset + subbit_length)] >> bit_right_amount
		return str(bitarray)

def add_bitarray_tests(bit_length):
	for i in xrange(NUM_TESTS_PER_BIT_LENGTH):
		init_bit_string = get_init_bit_string(bit_length)
		bit_offset = get_random_offset(bit_length)
		subbit_length = get_random_subbit_length(bit_length - bit_offset)	
		bit_right_amount = get_random_right_amount()
		expected_result = get_expected_result(init_bit_string, bit_offset, subbit_length, bit_right_amount)
		#print "TEST: ", init_bit_string, bit_offset, subbit_length, bit_right_amount, expected_result
		# how to do we determine all the variations of bit_offset, bit_length, bit_right_amount
		add_bitarray_test(init_bit_string, bit_offset, subbit_length, bit_right_amount, expected_result)

def parse_args():
	parser = ArgumentParser()
	parser.add_argument("bitarray_lengths", help="comma-separated lengths of bitarrays to generate tests for")
	parser.add_argument("num_tests", help="number of tests to generate for each bitarray length specified",type=int)
	return parser.parse_args()
	
def main():
	args = parse_args()
	global NUM_TESTS_PER_BIT_LENGTH
	global TEST_LIST

	NUM_TESTS_PER_BIT_LENGTH = args.num_tests
	bitarray_lengths = map(int, args.bitarray_lengths.split(','))

	for length in bitarray_lengths:
		add_bitarray_tests(length)
	test_string = "".join(TEST_LIST)
	
	default_test_file = open("curr_tests", 'w+')
	archive_test_file = open(str(int(round(time.time()))) + '_tests', 'w+')
	default_test_file.write(test_string)
	archive_test_file.write(test_string)


if __name__ == '__main__':
	main()