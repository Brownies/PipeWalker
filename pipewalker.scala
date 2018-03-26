import scala.volatile
import scala.math.max
import scala.collection.mutable.Buffer
import scala.collection.mutable.Set
import scala.collection.parallel.mutable.ParSeq
import java.awt.Point
/*
 * Some assumptions:
 * Every line in the input string is of equal length, i.e when split by '/n' the result is a rectangular 2D array.
 * 
 * Only the space character (' ', ASCII 0x20) will be used to denote empty or inaccessible tiles.
 * 
 * The graph is directed and one-way connections are possible. Consider the following input:
 * 		"#-|"
 * 		"  #"
 * The longest path is 4 steps from the end piece in the top left through the horizontal connector to the vertical connector and bottom right end piece.
 * There is no valid path starting from the bottom right end piece.
 */
class PipeWalker {
  // TODO: Add some private methods and modify longestPipe to solve the problem
  def longestPipe(inputText: String): Int = {
    val (grid, startPositions) = init(inputText)  
    return dfs(grid, startPositions/*.par*/)
  }

 
  case class Node(piece: Char, neighbours: List[Point]) {
    def toString() = piece.toString()
    def mkString() = toString()
  }
  
  
  private def dfs(grid: Array[Array[Node]], startPositions: Buffer[Point]):Int = {
    @volatile var longestPath = 0
    
    startPositions.foreach(p => inner(grid, Set(p), grid(p.y)(p.x), 1))
    
    def inner(grid: Array[Array[Node]], visited: Set[Point], currentNode: Node, depth: Int): Unit = {
      
      //println(currentNode.piece)
      if (currentNode.piece == '#') {
        longestPath = max(longestPath, depth)
      }
      
      for (neighbour <- currentNode.neighbours) {
        if (!visited.contains(neighbour)) {
          inner(grid, visited + neighbour, grid(neighbour.y)(neighbour.x), depth+1)
        }
      }
    }
    return longestPath
  }
  
  private def init(in: String): (Array[Array[Node]], Buffer[Point]) = {
    val arr = in.split("\n")
    val emptyNode = new Node(' ', List())
    var grid = Array.fill[Node](arr.length, arr(0).length)(emptyNode)
    val startPositions: Buffer[Point] = Buffer()
    
    for (y <- 0 until arr.length) {
      for (x <- 0 until arr(0).length) {
        arr(y)(x)  match {
          case ' ' => 
            
          case '-' => grid(y)(x) = new Node('-', getHorizontalNeighbours(y, x))
          
          case '|' => grid(y)(x) = new Node('|', getVerticalNeighbours(y, x))
          
          case '+' => grid(y)(x) = new Node('+', getHorizontalNeighbours(y, x) ++ getVerticalNeighbours(y, x))
          
          case '#' => {
            grid(y)(x) = new Node('#', getHorizontalNeighbours(y, x) ++ getVerticalNeighbours(y, x))
            startPositions.append(new Point(x, y))
          }
          
          case default =>
        }
      }
    }
    
    def getHorizontalNeighbours(y: Int, x: Int):List[Point] = {
      val ret = Buffer[Point]()
      if (x-1 >= 0 && x < arr(0).length && arr(y)(x-1) != ' ') {ret.append(new Point(x-1, y))}
      if (x >= 0 && x+1 < arr(0).length && arr(y)(x+1) != ' ') {ret.append(new Point(x+1, y))}
      return ret.toList
    }
    def getVerticalNeighbours(x: Int, y: Int):List[Point] = {
      val ret = Buffer[Point]()
      if (y-1 >= 0 && y < arr.length && arr(y-1)(x) != ' ') {ret.append(new Point(x, y-1))}
      if (y >= 0 && y+1 < arr.length && arr(y+1)(x) != ' ') {ret.append(new Point(x, y+1))}
      return ret.toList
    }
    
    return (grid, startPositions)
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

  private val level4 =
    "#++#"
    
  val tests = List(
    TestCase("", 0),
    //TestCase(level1, 9),
    //TestCase(level2, 6),
    //TestCase(level3, 11),
    TestCase(level4, 3)
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