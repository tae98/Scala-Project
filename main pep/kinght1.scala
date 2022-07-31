// Part 1 about finding and counting Knight's tours
//==================================================

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(1a) Complete the function that tests whether the position 
// is inside the board and not yet element in the path.

def is_legal(dim: Int, path: Path)(x: Pos): Boolean = {
  val isInside = (x._1 >= 0) && (x._1 < dim) && (x._2 >= 0) && (x._2 < dim)
  val isInPath = path.find(y => y == x).isDefined
  isInside && !isInPath
}

//DELETE ME BEFORE SUBMISSION
is_legal(5, List((1,3), (2,3), (1, 1)))((4, 4)) //True
is_legal(5, List((1,3), (2,3), (1, 1)))((2, 1)) //True
is_legal(5, List((1,3), (2,3), (1, 1)))((0, 0)) //True
is_legal(5, List((1,3), (2,3), (1, 1)))((5, 4)) //False: Not inside
is_legal(5, List((1,3), (2,3), (1, 1)))((5, 5)) //False: Not inside
is_legal(5, List((1,3), (2,3), (1, 1)))((-1, 1)) //False: Not inside
is_legal(5, List((1,3), (2,3), (1, 1)))((1, 1)) //False: In path

//(1b) Complete the function that calculates for a position 
// all legal onward moves that are not already in the path. 
// The moves should be ordered in a "clockwise" order.
 
def legal_moves(dim: Int, path: Path, x: Pos): List[Pos] = {
  val one = (x._1 + 1, x._2 + 2)
  val two = (x._1 + 2, x._2 + 1)
  val three = (x._1 + 2, x._2 - 1)
  val four = (x._1 + 1, x._2 - 2)
  val five = (x._1 - 1, x._2 - 2)
  val six = (x._1 - 2, x._2 - 1)
  val seven = (x._1 - 2, x._2 + 1)
  val eight = (x._1 - 1, x._2 + 2)

  List(one, two, three, four, five, six, seven, eight)
    .filter(move => is_legal(dim, path)(move))
}

assert(legal_moves(8, Nil, (2,2)) ==
  List((3,4), (4,3), (4,1), (3,0), (1,0), (0,1), (0,3), (1,4)))
assert(legal_moves(8, Nil, (7,7)) == List((6,5), (5,6)))
assert(legal_moves(8, List((4,1), (1,0)), (2,2)) ==
  List((3,4), (4,3), (3,0), (0,1), (0,3), (1,4)))
assert(legal_moves(8, List((6,6)), (7,7)) == List((6,5), (5,6)))


//(1c) Complete the two recursive functions below. 
// They exhaustively search for open tours starting from the 
// given path. The first function counts all possible open tours, 
// and the second collects all open tours in a list of paths.

def is_closed(dim: Int, path: Path): Boolean =
  legal_moves(dim, List(path.head), path.head).contains(path.last)


def count_tours(dim: Int, path: Path): Int = {
  val startingPos = path.head
  val legalMoves = legal_moves(dim, path, startingPos)
  if  ((path.size == dim*dim) && (!is_closed(dim, path))) 1
  else if  (path.size == dim*dim && is_closed(dim, path)) 0
  else
    (for (move <- legalMoves) yield count_tours(dim, move::path)).sum
}

count_tours(5, List((0,0)))

def enum_tours(dim: Int, path: Path): List[Path] = {
  val startingPos = path.head
  val legalMoves = legal_moves(dim, path, startingPos)
  if  ((path.size == dim*dim) && (!is_closed(dim, path))) List(path)
  else if  (path.size == dim*dim && is_closed(dim, path)) Nil
  else
    (for (move <- legalMoves) yield enum_tours(dim, move::path)).flatten
}


enum_tours(5, List((0, 0)))