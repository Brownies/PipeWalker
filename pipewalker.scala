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
 * The graph is directed and one-way connections are possible. Consider the following input:
 * 		"#-|\n" +
 * 		"  #"
 * The longest path is 4 steps from the end piece in the top left through the horizontal connector to the vertical connector and bottom right end piece.
 * There is no valid path starting from the bottom right end piece.
 */
class PipeWalker {
  def longestPipe(inputText: String): Int = {
    val (grid, startPositions) = init(inputText)  
    return dfs(grid, startPositions)
  }

 
  case class Node(piece: Char, neighbours: ParSeq[Point]) {
    override def toString() = piece.toString()
  }
  
  
  object Piece extends Enumeration {
    type Piece = Value
    val EMPTY = ' '
    val HORIZONTAL = '-'
    val VERTICAL = '|'
    val COMBO = '+'
    val END = '#'
  }
  
  
  private def dfs(grid: Array[Array[Node]], startPositions: Buffer[Point]):Int = {
    @volatile var longestPath = 0
    
    startPositions.par.foreach(p => inner(grid, Set(p), grid(p.y)(p.x), 1))
    
    def inner(grid: Array[Array[Node]], visited: Set[Point], currentNode: Node, depth: Int): Unit = {
      if (currentNode.piece == Piece.END) {
        longestPath = max(longestPath, depth)
      }
      
      currentNode.neighbours.foreach(neighbour => {
        if (!visited.contains(neighbour)) {
          inner(grid, visited + neighbour, grid(neighbour.y)(neighbour.x), depth+1)
        }
      })
    }
    return longestPath
  }
  
  
  private def init(in: String): (Array[Array[Node]], Buffer[Point]) = {
    val arr = in.split("\n")
    val emptyNode = new Node(Piece.EMPTY, ParSeq())
    val grid = Array.fill[Node](arr.length, arr(0).length)(emptyNode)
    val startPositions: Buffer[Point] = Buffer()
    
    for (y <- 0 until arr.length) {
      for (x <- 0 until arr(0).length) {
        arr(y)(x)  match {
          case Piece.EMPTY => 
            
          case Piece.HORIZONTAL => grid(y)(x) = new Node(Piece.HORIZONTAL, getHorizontalNeighbours(y, x))
          
          case Piece.VERTICAL => grid(y)(x) = new Node(Piece.VERTICAL, getVerticalNeighbours(y, x))
          
          case Piece.COMBO => grid(y)(x) = new Node(Piece.COMBO, getHorizontalNeighbours(y, x) ++ getVerticalNeighbours(y, x))
          
          case Piece.END => {
            grid(y)(x) = new Node(Piece.END, getHorizontalNeighbours(y, x) ++ getVerticalNeighbours(y, x))
            startPositions.append(new Point(x, y))
          }
          
          case default =>
        }
      }
    }
    
    def getHorizontalNeighbours(y: Int, x: Int): ParSeq[Point] = {
      val ret = Buffer[Point]()
      if (x-1 >= 0 && x < arr(0).length && arr(y)(x-1) != Piece.EMPTY) {ret.append(new Point(x-1, y))}
      if (x >= 0 && x+1 < arr(0).length && arr(y)(x+1) != Piece.EMPTY) {ret.append(new Point(x+1, y))}
      return ret.par
    }
    
    def getVerticalNeighbours(y: Int, x: Int): ParSeq[Point] = {
      val ret = Buffer[Point]()
      if (y-1 >= 0 && y < arr.length && arr(y-1)(x) != Piece.EMPTY) {ret.append(new Point(x, y-1))}
      if (y >= 0 && y+1 < arr.length && arr(y+1)(x) != Piece.EMPTY) {ret.append(new Point(x, y+1))}
      return ret.par
    }
    
    return (grid, startPositions)
  }
}


case class TestCase(testInput: String, expectedLength: Int)

object TestCases {

  private val level0 =
    "#"
  
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
    "#++#\n" + 
    " ++ "
  
  private val level5 = 
    "#+#\n" + 
    "|-|\n" + 
    "#+#"
    
  private val level6 =
    "#-|\n" + 
    "  #"
    
  private val level7 =
    "#---# #---#\n" + 
    "-----+-----\n" + 
    "#---# #---#"
    
  private val level8 =
    "+---+ ##\n" +
    "##  | | \n" +
    "# |-#-+ \n" +
    "----|   \n" +
    "#------#"
    
  private val level9 =
    "#---+--+\n" +
    "   |+--+\n" +
    "--------"
    
  private val level10 =
    "sdf_<>g++-"
    
  private val level999 = //parallelization test
    "##########\n" + 
    "##########\n" + 
    "##########\n" + 
    "##########\n" + 
    "##########\n" + 
    "##########\n" + 
    "##########\n" + 
    "##########\n" + 
    "##########\n" + 
    "##########\n"
    
  private val level9000 = //inner() parallelization test
    "#+++++++++\n" +
    "++++++++++\n" +
    "++++++++++\n" +
    "++++++++++\n" +
    "++++++++++\n" +
    "++++++++++\n" +
    "++++++++++\n" +
    "++++++++++\n" +
    "++++++++++\n" +
    "+++++++++#"

  val tests = List(
    TestCase("", 0),
    TestCase(level0, 1),
    TestCase(level1, 9),
    TestCase(level2, 6),
    TestCase(level3, 11),
    TestCase(level4, 6),
    TestCase(level5, 9),
    TestCase(level6, 4),
    TestCase(level7, 5),
    TestCase(level8, 24),
    TestCase(level9, 1),
    TestCase(level10, 0)
    
    //TestCase(level999, 100),
    //TestCase(level9000, 100)
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

  def printFailsOnly(results: List[TestResult]) =
    results.filter(_.didPass).map(formatResult).foreach(println)
    
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