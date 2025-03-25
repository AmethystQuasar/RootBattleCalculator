import RootBattleCalculator._
  
/** INFO
 * This calculator can be used to find the likelihoods for different outcomes
 * for battles in the board game Root by Cole Wehrle.
 *
 * Describe the beginning state of the clearing a fight is taking place using
 * the ClearingState class and the PlayerState class (see details).
 *
 * The calculator has four functions you can use, and they all take as parameters:
 * 1. the attacker name
 * 2. the defender name
 * 3. the ClearingState described by you
 * 4. the number of rolls the attacker can make
 *
 * The functions are:
 * eliminateChance, ruleChance, buildChance, and overthrowChance
 * (see details in RootBattleCalculator).
 *
 * Function results are printed to console.
 *
 * Though all of the functions can only handle a single attacker and defender,
 * finding the likelihood of an outcome for a chain of battles with different
 * opponents is easy enough, as you can simply multiply the outcome chances
 * together.
 *
 * With these methods, you should be able to find almost any relevant outcome for
 * a battle in Root (see PlayerState for details on buildings and tokens). While
 * there aren't direct accomodations for special situations you can usually translate
 * them to these parameters. For example, if you want to know the likelihood of gaining
 * a specific amount of points by destroying tokens and buildings, only state the
 * numbers and tokens you need to remove and use 'eliminateChance'.
 *
 * Let me know if you find errors or if I'm missing something!
 */

@main def RootBattleCalcApp() =
  val exampleState = ClearingState(2, Map(
    "Cherry" -> PlayerState(4, 2, 1),
    "Jenny" -> PlayerState(4, 0, 0),
    "Ashley" -> PlayerState(2, 0, 1)
  ))

  eliminateChance("Jenny", "Cherry", exampleState, 3)
  eliminateChance("Jenny", "Cherry", exampleState, 2)
  buildChance("Jenny", "Cherry", exampleState, 3)
  buildChance("Jenny", "Cherry", exampleState, 2)
  ruleChance("Jenny", "Cherry", exampleState, 2)
  overthrowChance("Jenny", "Cherry", exampleState, 2)
  overthrowChance("Jenny", "Cherry", exampleState, 1)
