// Preliminary Part about finding Knight's tours
//===============================================


object CW9a {

// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below. Also have a look whether the functions
// at the end are of any help.



type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(1) Complete the function that tests whether the position x
//    is inside the board and not yet element in the path.

def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
  val path_con = if (path.contains(x)) true else false
 if ((x._1)>=0 && (x._2)>=0 &&( x._1)< dim && (x._2)< dim && (path_con==false)) true 
 else false
} 



//(2) Complete the function that calculates for a position x
//    all legal onward moves that are not already in the path. 
//    The moves should be ordered in a "clockwise" manner.
 
def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  val clock_1 =(x._1+1, x._2+2)
  val clock_2 =(x._1+2, x._2+1)
  val clock_3 =(x._1+2, x._2-1)
  val clock_4 =(x._1+1, x._2-2)
  val clock_5 =(x._1-1, x._2-2)
  val clock_6 =(x._1-2, x._2-1)
  val clock_7 =(x._1-2, x._2+1)
  val clock_8 =(x._1-1, x._2+2)

  val lst = List(clock_1,clock_2,clock_3,clock_4,clock_5,clock_6,clock_7,clock_8)
  val result:List[Pos]=lst.filter(foo=>is_legal(dim,path,foo))
  result
}


//some testcases
//
//assert(legal_moves(8, Nil, (2,2)) == 
//  List((3,4), (4,3), (4,1), (3,0), (1,0), (0,1), (0,3), (1,4)))
//assert(legal_moves(8, Nil, (7,7)) == List((6,5), (5,6)))
//assert(legal_moves(8, List((4,1), (1,0)), (2,2)) == 
//  List((3,4), (4,3), (3,0), (0,1), (0,3), (1,4)))
//assert(legal_moves(8, List((6,6)), (7,7)) == List((6,5), (5,6)))


//(3) Complete the two recursive functions below. 
//    They exhaustively search for knight's tours starting from the 
//    given path. The first function counts all possible tours, 
//    and the second collects all tours in a list of paths.

def count_tours(dim: Int, path: Path) : Int = {
  val intialPos = path.head
  val legalMov = legal_moves(dim, path, intialPos)
  if ((dim*dim == path.size) && (legal_moves(dim,List(path.head),path.head).contains(path.last)))0
  else if ((dim*dim == path.size  )&& (!(legal_moves(dim,List(path.head),path.head).contains(path.last)))) 1
    else (for (foo <- legalMov) yield count_tours(dim, foo::path)).sum
}

def enum_tours(dim: Int, path: Path) : List[Path] = {
  val intialPos = path.head
  val legalMov = legal_moves(dim, path, intialPos)
  val lst = Nil
  if((dim*dim == path.size) && (!(legal_moves(dim,List(path.head),path.head).contains(path.last)))) List(path)
  else if ((dim*dim == path.size) && (legal_moves(dim,List(path.head),path.head).contains(path.last))) lst
  else (for (foo <- legalMov) yield enum_tours(dim, foo::path)).flatten
}


//(4) Implement a first-function that finds the first 
//    element, say x, in the list xs where f is not None. 
//    In that case Return f(x), otherwise None. If possible,
//    calculate f(x) only once.

def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = xs match {
  case Nil => None
  case xsh::xst => val function:Option[Path]=f(xsh)
  if (function!= None) function else first(xst,f)
}


// testcases
//
//def foo(x: (Int, Int)) = if (x._1 > 3) Some(List(x)) else None
//
//first(List((1, 0),(2, 0),(3, 0),(4, 0)), foo)   // Some(List((4,0)))
//first(List((1, 0),(2, 0),(3, 0)), foo)          // None


//(5) Implement a function that uses the first-function from (5) for
//    trying out onward moves, and searches recursively for a
//    knight tour on a dim * dim-board.

def first_tour(dim: Int, path: Path) : Option[Path] = {
  if (dim*dim == path.length) Some(path)
  else 
    {val result =first(legal_moves(dim,path,path.head), x => first_tour(dim,x+:path))
    result
    }
}


/* Helper functions


// for measuring time
def time_needed[T](code: => T) : T = {
  val start = System.nanoTime()
  val result = code
  val end = System.nanoTime()
  println(f"Time needed: ${(end - start) / 1.0e9}%3.3f secs.")
  result
}

// can be called for example with
//     time_needed(count_tours(dim, List((0, 0))))
// in order to print out the time that is needed for 
// running count_tours


// for printing a board
def print_board(dim: Int, path: Path): Unit = {
  println()
  for (i <- 0 until dim) {
    for (j <- 0 until dim) {
      print(f"${path.reverse.indexOf((j, dim - i - 1))}%3.0f ")
    }
    println()
  } 
}


*/
}
