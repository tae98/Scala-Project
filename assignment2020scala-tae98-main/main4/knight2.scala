// Core Part about finding a single tour for a board using the
// Warnsdorf Rule
//==============================================================

object CW9b {


// !!! Copy any function you need from file knight1.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

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


//(6) Complete the function that calculates a list of onward
//    moves like in (2) but orders them according to Warnsdorf’s 
//    rule. That means moves with the fewest legal onward moves 
//    should come first.

 

def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    val legal = legal_moves(dim, path,x)
    legal.sortBy(y=>legal_moves(dim,path,y).length)
}


//(7) Complete the function that searches for a single *closed* 
//    tour using the ordered_moves function from (6). This
//    function will be tested on a 6 x 6 board. 

def helper(x:Pos,y:Pos):Boolean={
(for (swap <- List((1,2),(1,-2),(-1,2),(-1,-2),(2,1),(2,-1),(-2,1),(-2,-1)))
     yield 
     (x._1 + swap._1 == y._1 && x._2 + swap._2 == y._2)).contains(true)
    
}

def first_closed_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
    val newDim= dim*dim
    val ph=path.head
    val pl= path.last
if(dim<=4) {None} 
else if(dim ==1) {Some(List((0,0)))}
else if (ordered_moves(dim,path,ph).size>0){first(ordered_moves(dim,path,ph),(newPos:Pos)=>first_closed_tour_heuristics(dim,newPos::path))}
else if (path.length==newDim && helper(ph,pl)) {Some(path)}
else None
}



//(8) Same as (7) but searches for *non-closed* tours. This 
//    version of the function will be called with dimensions of 
//    up to 30 * 30.



def first_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
    
  def helper2(dim: Int, posPath: List[Path]):Option[Path]={
  val newDim= dim*dim
  posPath match{
      case Nil =>None
      case xh::xt=> if (xh.length == newDim ) Some(xh) else helper2(dim,ordered_moves(dim,xh,xh.head).map(_::xh):::xt)
    }
 }
   helper2(dim,List(path))
  }
}






