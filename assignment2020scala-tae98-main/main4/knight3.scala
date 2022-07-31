// Finding a single tour on a "mega" board
//=========================================

object CW9c {

// !!! Copy any function you need from file knight1.scala !!!
// !!! or knight2.scala                                   !!! 
//
def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
  val path_con = if (path.contains(x)) true else false
 if ((x._1)>=0 && (x._2)>=0 &&( x._1)< dim && (x._2)< dim && (path_con==false)) true 
 else false
} 

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

def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = xs match {
  case Nil => None
  case xsh::xst => val function:Option[Path]=f(xsh)
  if (function!= None) function else first(xst,f)
}

def first_tour(dim: Int, path: Path) : Option[Path] = {
  if (dim*dim == path.length) Some(path)
  else 
    {val result =first(legal_moves(dim,path,path.head), x => first_tour(dim,x+:path))
    result
    }
}

def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    val legal = legal_moves(dim, path,x)
    legal.sortBy(y=>legal_moves(dim,path,y).length)
}
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.


type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(9) Implement a function that searches for a 
//    you have to be careful to write a tail-recursive version as this 
//    function will be called with dimensions of up to 70 * 70
//    and starting field (0, 0). It has to produce a solution within
//    30 seconds.


def tour_on_mega_board(dim: Int, path: Path) : Option[Path] ={
    val newDim=dim*dim
    def helper3(dim: Int, path: Path, pathList: List[Path]) :Option[Path]= pathList match{
    case  xh::xt=> if (xh.length == newDim) Some(xh)
    else helper3(dim, path,for(i<- ordered_moves(dim,xh,xh.head)) yield i+:xh)
    case Nil => None}
    helper3(dim,path,path::Nil)
}
}
