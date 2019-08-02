package experiments.cakePattern.comprehendTheCake

import scala.collection.mutable

// tokenizer splits expression in tokens
class Tokenizer {
	def tokenize(expr: String): Tokenizer.Tokens = expr.split(" ")
}
object Tokenizer {
	type Tokens = Array[String]
}

// evaluates a list of tokens to an Integer
trait TokenEval {
	def eval(tokens: Tokenizer.Tokens): Int
}

class SimpleEvaluator extends TokenEval {
	def eval(tokens: Tokenizer.Tokens): Int = {
		tokens match {
			case Array(left, "+", right) => left.toInt + right.toInt
			case Array(left, "*", right) => left.toInt * right.toInt
		}
	}
}

class Eval7Evaluator extends TokenEval {
	def eval(tokens: Tokenizer.Tokens): Int = 7
}

// only tokenize and evaluate when the expression is not yet cached
class CachedEval(cache: mutable.Map[String, Int], tokenizer: Tokenizer, tokenEval: TokenEval) {
	def eval(expr: String): Int = {
		cache.getOrElse(expr, {
			val tokens = tokenizer.tokenize(expr)
			val result = tokenEval.eval(tokens)
			cache += (expr -> result)
			result
		})
	}
}

// evaluates the expression using the CachedEvaluator
class Evaluator(cachedEval: CachedEval) {
	def eval(expr: String): Int = cachedEval.eval(expr)
}

object test {
	// standard form of testing. mock or give some implementation for underlying structures
	val mockedTokenizer = new Tokenizer
	val mockedEvaluator = new SimpleEvaluator
	val toTest = new CachedEval(mutable.Map(), mockedTokenizer, mockedEvaluator)
}
