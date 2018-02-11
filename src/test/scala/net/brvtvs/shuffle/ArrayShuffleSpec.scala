package net.brvtvs.shuffle

import net.brvtvs.shuffle.ArrayShuffle._
import org.scalatest.FlatSpec

class ArrayShuffleSpec extends FlatSpec {

	// (input, output)
	val testCases = Seq(
		(Array(0, 1), Array(0, 1)),
		(Array(0, 1, 2, 3), Array(0, 2, 1, 3)),
		(Array(0, 1, 2, 3, 4, 5), Array(0, 3, 1, 4, 2, 5)),
		(Array(0, 1, 2, 3, 4, 5, 6, 7), Array(0, 4, 1, 5, 2, 6, 3, 7)),
		(Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), Array(0, 5, 1, 6, 2, 7, 3, 8, 4, 9)),
		(Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19), Array(0, 10, 1, 11, 2, 12, 3, 13, 4, 14, 5, 15, 6, 16, 7, 17, 8, 18, 9, 19))
	)

	"linearSpaceShuffle" should ", given an array of 2n elements a1, a2, ..., an, b1, b2, ..., bn shuffle the array into a1, b1, a2, b2, ..., an, bn" in {
		for ((input, output) <- testCases) {
			assert(linearSpaceShuffle(input.clone()).toList == output.toList)
		}
	}

	"attemptedConstantSpaceShuffle" should ", given an array of 2n elements a1, a2, ..., an, b1, b2, ..., bn shuffle the array into a1, b1, a2, b2, ..., an, bn" in {
		for ((input, output) <- testCases) {
			assert(attemptedConstantSpaceShuffle(input.clone()).toList == output.toList)
		}
	}

	"constantHeapSpaceShuffle" should ", given an array of 2n elements a1, a2, ..., an, b1, b2, ..., bn shuffle the array into a1, b1, a2, b2, ..., an, bn" in {
		for ((input, output) <- testCases) {
			assert(constantHeapSpaceShuffle(input.clone()).toList == output.toList)
		}
	}

	it should " agree with linearSpaceShuffle" in {
		for ((input, output) <- testCases) {
			assert(constantHeapSpaceShuffle(input.clone()).toList == linearSpaceShuffle(input.clone()).toList)
		}
	}

}
