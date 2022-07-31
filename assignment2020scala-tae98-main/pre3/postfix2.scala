// Shunting Yard Algorithm 
// including Associativity for Operators 
// =====================================

object CW8b {


// type of tokens
type Toks = List[String]

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// left- and right-associativity
abstract class Assoc
case object LA extends Assoc
case object RA extends Assoc


// power is right-associative,
// everything else is left-associative
def assoc(s: String) : Assoc = s match {
  case "^" => RA
  case _ => LA
}


// the precedences of the operators
val precs = Map("+" -> 1,
  		"-" -> 1,
		"*" -> 2,
		"/" -> 2,
                "^" -> 4)

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/", "^")

def is_op(op: String) : Boolean = {
if (ops.contains(op)) true
else false}

def prec(op1: String, op2: String) : Boolean =op1 match {
  case "/" | "*" | "+" | "-"=> precs(op2) >= precs(op1)
  case _ => precs(op2) > precs(op1)
}



def grab(op : String, st : Toks, empt : Toks = Nil):Toks = { 
	if(st != Nil && is_op(st.head) && prec(st.head, op) ){grab(op,st.tail, empt ++: List(st.head))}
	else empt 
}

def sort2(op : String, st : Toks):Boolean={
	if (grab(op,st) != Nil) true
	else false
}

def add(x:Int,y:Int):Int= x + y
def minus(x:Int,y:Int):Int= x - y
def mul(x:Int,y:Int):Int= x * y
def div(x:Int,y:Int):Int= x / y


def power(x: Int, n: Int) : Int =
  if (n == 0) 1 else x * power(x, n - 1) 


// (3) Implement the extended version of the shunting yard algorithm.
// This version should properly account for the fact that the power 
// operation is right-associative. Apart from the extension to include
// the power operation, you can make the same assumptions as in 
// basic version.

def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = toks match{
  case Nil => List.concat(out,st)
  
  case th::tt => th match{
  case "(" => {syard(tt,th+:st,out)}
  
  case ")" => {val foo = for (i<-(0 until st.indexOf("(")).toList)yield(st(i))
  syard(tt,st.drop(foo.length+1),List.concat(out, foo))}

  case "+" | "-" | "*" | "/" | "^" => {
      if(st == Nil) syard(tt,th+:st,out)
          else {if(is_op(st.head)){
                  if(prec(th,st.head)) {syard(toks,st.tail,out:+st.head)}
                       else {syard(tt,th+:st,out)}
               }else {syard(tt,th+:st,out)}
                }
              }
   case _ => syard(tt,st,out:+th)
  }
}

// test cases
// syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3"))  // 3 4 8 * 5 1 - 2 3 ^ ^ / +


// (4) Implement a compute function that produces an Int for an
// input list of tokens in postfix notation.

def compute(toks: Toks, st: List[Int] = Nil) : Int = {toks match{
case Nil => st.head
case th::tt => th match{
  case "+" => compute(tt, (add(st(0),st(1))+:st.drop(2)))
  case "-" => compute(tt, (minus(st(1),st(0))+:st.drop(2)))
  case "*" => compute(tt, (mul(st(0),st(1))+:st.drop(2)))
  case "/" => compute(tt, (div(st(1),st(0))+:st.drop(2)))
  case "^" => compute(tt,(power(st(1),st(0))+:st.drop(2)))
  case _ => compute(tt,th.toInt+:st)
}
}
}

// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))   // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15
// compute(syard(split("4 ^ 3 ^ 2")))      // 262144
// compute(syard(split("4 ^ ( 3 ^ 2 )")))  // 262144
// compute(syard(split("( 4 ^ 3 ) ^ 2")))  // 4096
// compute(syard(split("( 3 + 1 ) ^ 2 ^ 3")))   // 65536

}
