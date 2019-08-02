package experiments.cakePattern.comprehendTheCake

import scala.collection.mutable

trait TokenEvalProvider {
	def tokenEval: TokenEval
}

trait SimpleEvaluatorProviderImpl extends TokenEvalProvider {
	override final def tokenEval: TokenEval = new SimpleEvaluator
}

trait Eval7EvaluatorProviderImpl extends TokenEvalProvider {
	override final def tokenEval: TokenEval = new Eval7Evaluator
}

trait TokenizerProvider {
	final def tokenizer = new Tokenizer
}

trait CachedEvalProvider {
	this: TokenizerProvider with TokenEvalProvider =>

	final def cachedEval = new CachedEval(mutable.Map(), tokenizer, tokenEval)
}

trait EvaluatorProvider {
	this: CachedEvalProvider =>

	final def evaluator = new Evaluator(cachedEval)
}

object SimpleTest extends App {

	object Injector extends EvaluatorProvider
										 with CachedEvalProvider
										 with TokenizerProvider
										 with Eval7EvaluatorProviderImpl
	val evaluator = Injector.evaluator

	val onePlusOne = evaluator.eval("1 + 1")
	val sevenTimesThree = evaluator.eval("7 * 3")

	println(onePlusOne)
	println(sevenTimesThree)
}

object Eval7Test extends App {

	object Injector extends EvaluatorProvider
										 with CachedEvalProvider
										 with TokenizerProvider
										 with Eval7EvaluatorProviderImpl
	val evaluator = Injector.evaluator

	val value = evaluator.eval("1 + 1")

	println(value)
}

object DependencyLifeTime {

	class InjectorFactory extends EvaluatorProvider
																with CachedEvalProvider
																with TokenizerProvider
																with Eval7EvaluatorProviderImpl

	val eval1 = new InjectorFactory().evaluator
	val eval2 = new InjectorFactory().evaluator
}

object SharedCacheEval {
	private val singleCache = mutable.Map[String, Int]()

	trait Provider extends CachedEvalProvider {
		this: TokenizerProvider with TokenEvalProvider =>
		final def cacheEval = new CachedEval(singleCache, tokenizer, tokenEval)
	}

	object Injector extends EvaluatorProvider
													with Provider
													with TokenizerProvider
													with Eval7EvaluatorProviderImpl

	// these use the same cache
	val eval1 = Injector.evaluator
	val eval2 = Injector.evaluator
}
