package net.brvtvs.shuffle

/**
	* Solutions to a problem from my friend's Algorithms course.
	* <p>
	* The problem: Given an array of 2n elements a1, a2, ..., an, b1, b2, ..., bn. Give an algorithm to shuffle the array
	* in place (without using more than O(1) space) into a1, b1, a2, b2, ..., an, bn. Demonstrate your algorithm is
	* correct and analyze the time complexity of the algorithm.
	*/
object ArrayShuffle {

	// First, I created this simple implementation so that I could test other more efficient implementations against it.
	// This shuffles the array by putting the output in a new array. The space complexity and computational complexity are
	// both O(n) where n = the size of the array.
	def linearSpaceShuffle[A](arr: Array[A]): Array[A] = {
		val output = arr.clone()
		for (i <- arr.indices) {
			output.update(newIndex(i, arr.length), arr(i))
		}
		output
	}

	// My first attempt at making a shuffle implementation with space complexity O(1). I originally discounted recursion,
	// because divide-and-conquer recursion scales stack frames logarithmically with the size of the array, and thus is
	// not O(1) space complexity. It turns out though that the person who created the question meant O(1) for heap space
	// complexity, not stack space complexity, which they did not initially make clear :/
	//
	// This attempt tries to swap values in place in the array, storing only a single buffered value while the swapping is
	// going on. In other words, if the value index 1 needs to be swapped to index 2, then I do that and, having displaced
	// the value at index 2, figure out where the value at index 2 is supposed to go (and then find the place for what
	// that value will displace). And so on until encountering an index that had already been swapped.
	def attemptedConstantSpaceShuffle[A](arr: Array[A]): Array[A] = {
		// This attempt first used O(n) approach to determine if an index had already been shuffled. I then tried to
		// establish a logical/mathematical function that could be used to determine whether an index had already
		// been swapped, without having to store a flag for every index. Figuring out that function would have made this
		// solution truly O(1) space complexity. But if such a function does exist, it is very complex and I did not find it
		// before I found out that the intended solution to this problem is just to use recursion.
		val updated = new Array[Boolean](arr.length)

		for (start <- arr.indices.filter(x => x % 2 == 1 && x < arr.length - 1)) {
			var first = true
			var i = start
			var value = arr(i)
			while (!updated(newIndex(i, arr.length))) {
				// gets the place where $value will be swapped to.
				val newI = newIndex(i, arr.length)
				// records the value that we are replacing with $value so that we can figure out where this should subsequently be put
				val displaced = arr(newI)
				arr.update(newI, value)

				if (!first) {
					// records that this index has been fully handled and should not be swapped again
					updated.update(newI, true)
				}
				first = false

				i = newI
				value = displaced
			}
		}

		arr
	}

	// This is very dumb, but TECHNICALLY it meets the requirements of the problem. It has O(1) heap space complexity and
	// O(n) time complexity. Sure in REAL life this would be extremely stupid and would cause a stack overflow for any
	// large input. But I guess that's what you get when you provide overly simplistic constraints on a problem like that
	// stack memory does not matter but heap memory does.
	def constantHeapSpaceShuffle[A](arr: Array[A], currentIdx: Int = 0): Array[A] = {
		// base case: end of the array
		if (currentIdx >= arr.length) arr
		else {
			// records the value at $currentIdx and where it should be shifted to
			val value = arr(currentIdx)
			val newIdx = newIndex(currentIdx, arr.length)

			//recursive call to handle the next position in the array.
			constantHeapSpaceShuffle(arr, currentIdx + 1)

			// this will only be evaluated after $value has been stored in a stack frame for every index in the array, so we
			// can start now start updating the array without fear of losing any data
			arr.update(newIdx, value)
		}
		arr
	}

	/**
		* Gets where an index should be relocated to in the shuffle.
		*
		* @param index   The pre-shuffled index.
		* @param arrSize The total size of the array being shuffled.
		* @return The new index.
		*/
	def newIndex(index: Int, arrSize: Int): Int = {
		val half = arrSize / 2
		if (index < half) {
			// first half of the array
			index * 2
		} else {
			// second half of the array
			val secondHalfIndex = index - half
			index - (half - secondHalfIndex - 1)
		}
	}

}
