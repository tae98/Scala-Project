// Advanvced Part 3 about really dump investing strategy
//=======================================================

//two test portfolios

val blchip_portfolio = List("GOOG", "AAPL", "MSFT", "IBM", "FB", "YHOO", "AMZN", "BIDU")
val rstate_portfolio = List("PLD", "PSA", "AMT", "AIV", "AVB", "BXP", "CBG", "CCI", 
                            "DLR", "EQIX", "EQR", "ESS", "EXR", "FRT", "GGP", "HCP") 


// (1) The function below should obtain the first trading price
// for a stock symbol by using the query
//
//    http://ichart.yahoo.com/table.csv?s=<<symbol>>&a=0&b=1&c=<<year>>&d=1&e=1&f=<<year>> 
// 
// and extracting the first January Adjusted Close price in a year.
import io.Source
import scala.util.matching.Regex
def get_first_price(symbol: String, year: Int): Option[Double] = {
	val url = "http://ichart.yahoo.com/table.csv?s=" + symbol + "&a=0&b=1&c=" + year + "&d=1&e=1&f=" + year
	try { 
		val listStrings = Source.fromURL(url).mkString.split("\n").toList.reverse
  		val firstTrade = listStrings.head.split(",").toList
  		val adjPrice = firstTrade(6).toDouble
  		Option(adjPrice)
	} catch {
		case e: Exception => None
	}
}

// Complete the function below that obtains all first prices
// for the stock symbols from a portfolio for the given
// range of years

def get_prices(portfolio: List[String], years: Range): List[List[Option[Double]]] = {
	var outerList = List[List[Option[Double]]]()
	for( j <- years) {
		var innerList = List[Option[Double]]()
		for( i <- portfolio) 
			innerList ::= get_first_price(i, j) 
		
		outerList ::= innerList.reverse
	}
	outerList.reverse
}

// test case
val p = get_prices(List("GOOG", "AAPL"), 2010 to 2012)


// (2) The first function below calculates the change factor (delta) between
// a price in year n and a price in year n+1. The second function calculates
// all change factors for all prices (from a portfolio).

def get_delta(price_old: Option[Double], price_new: Option[Double]): Option[Double] = {
	if(price_new.isDefined && price_old.isDefined)
		Option((price_new.get - price_old.get)/price_old.get)
	else None
}

def get_deltas(data: List[List[Option[Double]]]):  List[List[Option[Double]]] = {
	var deltas = List[List[Option[Double]]]()
	var list = List[Option[Double]]()
	var innerList = List[Option[Double]]()
	var nextList = List[Option[Double]]()

    for( innerListIndex <- 0 until data.size-1) {
    	for( i <- data(innerListIndex).indices) {
    		innerList = data(innerListIndex)
    		nextList = data(innerListIndex + 1)
    		list ::= get_delta(innerList(i), nextList(i))
    	}
    	deltas ::= list.reverse
    	list = List[Option[Double]]()
    }
	deltas.reverse
}

// test case using the prices calculated above
val d = get_deltas(p)


// (3) Write a function that given change factors, a starting balance and a year
// calculates the yearly yield, i.e. new balanace, according to our dump investment 
// strategy. Another function calculates given the same data calculates the
// compound yield up to a given year. Finally a function combines all 
// calculations by taking a portfolio, a range of years and a start balance
// as arguments.

def yearly_yield(data: List[List[Option[Double]]], balance: Long, year: Int): Long = {
	val yearList = data(year)
	var size = 0
	var investmentYield = 0.0

	for( j <- yearList)
		if(j.isDefined)
			size = size + 1

	val amountPerStock = balance/size
	
	for( i <- yearList.indices)
		if (yearList(i).isDefined)
			investmentYield += amountPerStock * yearList(i).get

	balance + investmentYield.toLong
}

//test case
yearly_yield(d, 100, 0)

def compound_yield(data: List[List[Option[Double]]], balance: Long, year: Int): Long = {
	var newBalance = balance
	for( i <- 0 until year) 
		newBalance = yearly_yield(data, newBalance, i)
	
	newBalance
}

def investment(portfolio: List[String], years: Range, start_balance: Long): Long = {
	val z = get_deltas(get_prices(portfolio, years))
	compound_yield(z, start_balance, years.end - years.start)
}


//test cases for the two portfolios given above
investment(rstate_portfolio, 1978 to 2016, 100)
investment(blchip_portfolio, 1978 to 2016, 100)