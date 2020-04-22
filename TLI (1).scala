import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

// Create a class called Expr
class Expr(op1: Any, operator: Any, op2: Any){
	
	override def toString = {
		if (op2 == None){
			operator + " " + op1
		} else {
			op1 + " " + operator + " " + op2
		}
	}
	
	def eval(symTable: Map[Any,Any]) :Any = {
		
		if (op1.toString().toDoubleOption != None){
			symTable += (op1 -> op1.toString().toDouble)
		}
		if (op2.toString().toDoubleOption != None){
			symTable += (op2 -> op2.toString().toDouble)
		}
		
		if (operator == "var"){
			val result = symTable(op1)
			return result
		} else if (operator == "string"){
			var newOp1 = op1.asInstanceOf[String].replaceAll("\"", "")
			return newOp1.asInstanceOf[String]
		}  else if (operator == "constant"){
			return op1.asInstanceOf[Double]
		} else if (operator == "+"){
			val first = symTable(op1).asInstanceOf[Double]
			val second = symTable(op2).asInstanceOf[Double]
			val result = first + second
			return result
		} else if (operator == "-"){
			val first = symTable(op1).asInstanceOf[Double]
			val second = symTable(op2).asInstanceOf[Double]
			val result = first - second
			return result
		} else if (operator == "*"){
			val first = symTable(op1).asInstanceOf[Double]
			val second = symTable(op2).asInstanceOf[Double]
			val result = first * second
			return result
		} else if (operator == "/"){
			val first = symTable(op1).asInstanceOf[Double]
			val second = symTable(op2).asInstanceOf[Double]
			val result = first / second
			return result
		} else if (operator == "<"){
			val first = symTable(op1).asInstanceOf[Double]
			val second = symTable(op2).asInstanceOf[Double]
			val result = first < second
			return result
		} else if (operator == ">"){
			val first = symTable(op1).asInstanceOf[Double]
			val second = symTable(op2).asInstanceOf[Double]
			val result = first > second
			return result
		} else if (operator == "<="){
			val first = symTable(op1).asInstanceOf[Double]
			val second = symTable(op2).asInstanceOf[Double]
			val result = (first <= second)
			return result
		} else if (operator == ">="){
			val first = symTable(op1).asInstanceOf[Double]
			val second = symTable(op2).asInstanceOf[Double]
			val result = (first >= second)
			return result
		} else if (operator == "=="){
			val first = symTable(op1).asInstanceOf[Double]
			val second = symTable(op2).asInstanceOf[Double]
			val result = (first == second)
			return result
		} else if (operator == "!="){
			val first = symTable(op1).asInstanceOf[Double]
			val second = symTable(op2).asInstanceOf[Double]
			val result = (first != second)
			return result
		} else {
			println("Syntax error on line " + (symTable("sListLineCount").asInstanceOf[Int] + 1).toString() + ".")
    			System.exit(0)
		}
	}
}

// Create a class called Stmt
class Stmt(keyword: String, exprs: Array[String]){

	def copy(newKeyword: String = keyword, newExprs: Array[String] = exprs): Stmt =
		new Stmt(newKeyword, newExprs)
		
	var others = ""
	exprs.foreach(exp => others = others + " " + exp.mkString(" "))
	
	override def toString = keyword + others
	
	def perform(symTable: Map[Any,Any]){
		
		// LET
		// sets expressions to their values in the symTable
		if (keyword == "let"){
			var tempExprs: Any = 0
			if (exprs.size > 3){
				tempExprs = exprs(2)
				var internal = new Expr(exprs(2), exprs(3), exprs(4)).eval(symTable)
				exprs(2) = internal.toString()
			}
			
			if (exprs(2).toIntOption != None){
				symTable += (exprs(0) -> exprs(2).toDouble)	
			} else{
				while (exprs(2).toDoubleOption == None){
					exprs(2) = symTable(exprs(2)).toString()
				}
				
				if (exprs(2).toDoubleOption != None){
					symTable += (exprs(0) -> exprs(2).toDouble)
				}
			}
			if (tempExprs != 0){
				exprs(2) = tempExprs.toString()
			}
			
		// PRINT
		// prints out given expressions
		} else if (keyword == "print"){
			var finalPrint = ""
			
			var count = 0
			for (expressions <- exprs){
				
				var expressionsList = expressions.split("\\s+")
				if (expressionsList.size == 3){
					var evaluatedExpression = new Expr(expressionsList(0),expressionsList(1),expressionsList(2)).eval(symTable)
					symTable += (expressions -> evaluatedExpression.toString())
					count += 1
				}
			}
			
			var index = 0
			for (expressions <- exprs){
				var evaluatedExpressions: Any = 0
				if (symTable.contains(exprs(index))){
					evaluatedExpressions = new Expr(exprs(index),"var",None).eval(symTable)
				} else if (!symTable.contains(exprs(index)) && exprs(index).contains("\"")) {
					evaluatedExpressions = new Expr(exprs(index),"string",None).eval(symTable)
				} else if (!symTable.contains(exprs(index)) && !exprs(index).contains("\"")) {
					evaluatedExpressions = new Expr(exprs(index),"constant",None).eval(symTable)
				}
				finalPrint = finalPrint + evaluatedExpressions.toString() + " "
				index += 1
			}
			println(finalPrint.dropRight(1))
		
		// INPUT
		// takes input from user and sets the value into the symTable
		} else if (keyword == "input"){
			for (expressions <- exprs){
				val scanner = new java.util.Scanner(System.in)
				var newInput = scanner.nextLine()
				if (newInput.toString().toDoubleOption != None){
					var newerInput = newInput.toDouble
					symTable += (expressions -> newerInput)
				} else{
					println("Illegal or missing input")
    				System.exit(0)
				}
			}
		// IF
		// checks if statement and if true jumps to labeled statement
		} else if (keyword == "if"){
			var label = exprs(4)
			var exprCheck = new Expr(exprs(0),exprs(1),exprs(2)).eval(symTable)
			if (exprCheck == true) {
				symTable += ("sListLineCount" -> symTable(label).asInstanceOf[Int])
			}else {
				var updateCount = symTable("sListLineCount").asInstanceOf[Integer]
				updateCount += 1
				symTable += ("sListLineCount" -> updateCount)
			}
		}
		
		// incase IF isn't used, have to increment what line is currently being performed in
		// the symTable
		if (keyword != "if"){
			var updateCount = symTable("sListLineCount").asInstanceOf[Integer]
			updateCount += 1
			symTable.update("sListLineCount", updateCount)
		}		
	}
}

object TLI {

	// MAIN
	// Preprocesses data and then sends the parsed statements and expressions into the
	// symTable. After our statements are ready, it performs each statement line by line
    def main(args: Array[String]) {
    	// symbolTable dictionary
    	var symTable:Map[Any,Any] = Map()
    	
    	// sList  is our list of Statements
    	var sList = Array[Stmt]()
    	
    	// just a counter so we can see what line we're on when we throw errors
    	var lineCount = 0
    	
    	// keeps track of which index in our list of statements we are currently
    	// performing.
    	symTable.addOne("sListLineCount" -> 0)
    	
    	// Takes in our .txt file, and for each line in the file, we run the for loop:
    	var filename = args(0)
    	
		for (lines <- Source.fromFile(filename).getLines) {
    		
    		// Create an empty array,then add strings to array separating on whitespace
    		var line = lines.trim	
    		var stringList = Array[String]()
    		
    		stringList = line.split("\\s+")

    		val labelCheck = stringList(0).contains(":")
    		
    		// Check if string has a label
    		if (labelCheck == true){
    			var label = stringList(0).dropRight(1)
    			symTable += (label -> lineCount)
    			stringList = stringList.drop(1)
    		}
    		
    		// set keyword to the first element in stringList, create an array of expressions,
    		// error check for invalid keywords - print an error if it sees an invalid keyword
    		var keyword = stringList(0)
    		var expression = Array[String]()
    		var legalKeywords = Array("let", "if", "print", "input")
    		val keywordCheckBool = legalKeywords.contains(keyword)
    		
    		if (keywordCheckBool == false && !line.isEmpty()) {
    			println("Syntax error on line " + (lineCount + 1) + ".")
    			System.exit(0)
    		}

			// remove keyword from stringList and set it to a new array called stringLists
    		var stringLists = stringList.filter(! _.contains(keyword))
    		
    		// Check if print is the keyword, if so, manipulate the expressions so we print
    		// properly when we have different situations - 1 expression vs many or different
    		// types of expressions
    		if (keyword == "print") {
    			var printExprs = ""
    			
    			stringLists.foreach(strings => printExprs += strings + " " )
    			
    			var printExprsAfter = printExprs.dropRight(1)
    			var printExprsList = printExprsAfter.split(" , ").map(_.toString).distinct
    			
    			printExprsList.foreach(expressions => expression = expression:+ expressions)
    		} else {
    			stringLists.foreach(strings => expression = expression:+ strings)
    		} 
    		
    		// finally create our statments
			var statement = new Stmt(keyword, expression)
			
			sList = sList:+ statement
			
    		lineCount += 1
		}
		
    	var count = 0
    	var sListLen = sList.size
    	
    	// PERFORM
    	// perform our statements
    	while (count < sListLen){
    		var sListLineCount = symTable("sListLineCount").asInstanceOf[Integer]
    		sList(sListLineCount).perform(symTable)
    		count = sListLineCount + 1
    	}
    }
}
