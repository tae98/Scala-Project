// Part 2 about finding a single tour for a board
//================================================

// copy any function you need from file knight1.scala

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

def is_legal(dim: Int, path: Path)(x: Pos): Boolean = {
  val isInside = (x._1 >= 0) && (x._1 < dim) && (x._2 >= 0) && (x._2 < dim)
  val isInPath = path.find(y => y == x).isDefined
  isInside && !isInPath
}

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

//(2a) Implement a first-function that finds the first 
// element, say x, in the list xs where f is not None. 
// In that case return f(x), otherwise none.

def first(xs: List[Pos], f: Pos => Option[Path]): Option[Path] = xs match {
  case Nil => None
  case x::xs => {
    val rec = f(x)
    if (rec.isDefined) rec else first(xs, f)
  }
}

//(2b) Implement a function that uses the first-function for
// trying out onward moves, and searches recursively for an 
// *open* tour on a dim * dim-board.

def first_tour(dim: Int, path: Path): Option[Path] = {
  if (path.length == dim * dim) Some(path)
  else
    first(legal_moves(dim, path, path.head), x => first_tour(dim, x::path))
}

//first_tour(6, List((0, 0)))
first_tour(8, List((0,0)))