// Shunting Yard Algorithm
// by Edsger Dijkstra
// ========================

object CW8a {

// type of tokens
type Toks = List[String]

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/")

// the precedences of the operators
val precs = Map("+" -> 1,
		"-" -> 1,
		"*" -> 2,
		"/" -> 2)

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList


// (1) Implement below the shunting yard algorithm. The most
// convenient way to this in Scala is to implement a recursive 
// function and to heavily use pattern matching. The function syard 
// takes some input tokens as first argument. The second and third 
// arguments represent the stack and the output of the shunting yard 
// algorithm.
//
// In the marking, you can assume the function is called only with 
// an empty stack and an empty output list. You can also assume the
// input os  only properly formatted (infix) arithmetic expressions
// (all parentheses will be well-nested, the input only contains 
// operators and numbers).

// You can implement any additional helper function you need. I found 
// it helpful to implement two auxiliary functions for the pattern matching:  
// 

def is_op(op: String) : Boolean = {
if (ops.contains(op)) true
else false}

def prec(op1: String, op2: String) : Boolean = {
	if (precs(op1)>=precs(op2)) true 
	else false
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

def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = {
if(toks != Nil) toks.head match{
case "*" => if (sort2 ("*", st) ) syard(toks.tail, List.concat(List("*") ,st.takeRight (st.length - grab ("*", st).length)), List.concat(out, grab ("*", st)) )
else syard(toks.tail, List.concat(List("*") , st), out) 

case "/" => if (sort2 ("/", st) ) syard(toks.tail, List.concat(List("/") ,st.takeRight (st.length - grab ("/", st).length)), List.concat(out, grab ("/", st)) )
else syard(toks.tail, List.concat(List("/") , st), out) 

case "+" => if (sort2 ("+", st) ) syard(toks.tail, List.concat(List("+") ,st.takeRight (st.length - grab ("+", st).length)), List.concat(out, grab ("+", st)) )
else syard(toks.tail, List.concat(List("+") , st), out) 

case "-" => if (sort2 ("-", st) ) syard(toks.tail, List.concat(List("-") ,st.takeRight (st.length - grab ("-", st).length)), List.concat(out, grab ("-", st)) )
else syard(toks.tail, List.concat(List("-") , st), out) 

case "(" => syard (toks.tail, List.concat(List ("("), st), out)
case ")" => syard (toks.tail, st.takeRight (st.length - (st.indexOf ("(") + 1) ), List.concat(out, st.slice (0, st.indexOf ("(") ) ))
case num => if(toks.length==1) syard(Nil, Nil, List.concat(out, List(num), st)) else syard(toks.tail, st, List.concat(out, List(num)) )
}
else List.concat(out,st)
}


// test cases
//syard(split("3 + 4 * ( 2 - 1 )"))  // 3 4 2 1 - * +
//syard(split("10 + 12 * 33"))       // 10 12 33 * +
//syard(split("( 5 + 7 ) * 2"))      // 5 7 + 2 *
//syard(split("5 + 7 / 2"))          // 5 7 2 / +
//syard(split("5 * 7 / 2"))          // 5 7 * 2 /
//syard(split("9 + 24 / ( 7 - 3 )")) // 9 24 7 3 - / +
//syard(split("3 + 4 + 5"))           // 3 4 + 5 +
//syard(split("( ( 3 + 4 ) + 5 )"))    // 3 4 + 5 +
//syard(split("( 3 + ( 4 + 5 ) )"))    // 3 4 5 + +
//syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )")) // 3 4 5 + +

 
// (2) Implement a compute function that evaluates an input list
// in postfix notation. This function takes a list of tokens
// and a stack as argumenta. The function should produce the 
// result as an integer using the stack. You can assume 
// this function will be only called with proper postfix 
// expressions.    

def compute(toks: Toks, st: List[Int] = Nil) : Int = {
	if (toks != Nil) toks.head match{
		case "+" => compute(toks.tail,List.concat(st.take(st.length-2), List(add(st(st.length-2),(st.last)))))
		case "-" => compute(toks.tail, List.concat(st.take(st.length-2),List(minus(st(st.length-2),(st.last)))))
		case "*" => compute(toks.tail, List.concat(st.take(st.length-2),List(mul(st(st.length-2),(st.last)))))
		case "/" => compute(toks.tail, List.concat(st.take(st.length-2),List(div(st(st.length-2),(st.last)))))
		case num => compute(toks.tail, List.concat(st, List(num.toInt)))
		}
	else st.head
}


// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))  // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15

}


