class PipeWalker {
  // TODO: Add some private methods and modify longestPipe to solve the problem
  def longestPipe(inputText: String): Int = {
    0
  }
}


case class TestCase(testInput: String, expectedLength: Int)

object TestCases {
  // TODO: You probably want to add more test cases here

  private val level1 =
    "  +--+   \n" +
    "#-+  |   \n" +
    "     #   "

  private val level2 =
    " #----#   \n" +
    "          \n" +
    " #--#     "

  private val level3 =
    "   #-+---# \n" +
    "     |     \n" +
    " #-# +---# "

  val tests = List(
    TestCase("", 0),
    TestCase(level1, 9),
    TestCase(level2, 6),
    TestCase(level3, 11)
  )
}

// You can use this mini test framework or roll your own
class PipeWalkerTest(walker: PipeWalker) {
  def test(tests: List[TestCase]): List[TestResult] = tests.map(test => {
    val actualLength = walker.longestPipe(test.testInput)
    TestResult(test, actualLength, actualLength == test.expectedLength)
  })

  def printResults(results: List[TestResult]): Unit =
    results.map(formatResult).foreach(println)

  private def formatResult(result: TestResult): String = {
    val test = result.testCase
    val passText = if (result.didPass) "PASSED" else "FAILED"
    s"\nWith input:\n${test.testInput}" +
    s" length is ${result.actualLength}" +
    s" (expected ${test.expectedLength}) [" + passText + "]"
  }

  case class TestResult(testCase: TestCase, actualLength: Int, didPass: Boolean)
}


object Main extends App {
  val walker = new PipeWalker
  val tester = new PipeWalkerTest(walker)
  val results = tester.test(TestCases.tests)
  tester.printResults(results)
}