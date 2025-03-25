import java.text.DecimalFormat
import scala.math._

object RootBattleCalculator {

  private val twoDecimals = DecimalFormat("#.##")
  private val allRolls: Array[(Int, Int)] = Array(
    (0, 0),
    (1, 0), (1, 0),
    (1, 1),
    (2, 0), (2, 0),
    (2, 1), (2, 1),
    (2, 2),
    (3, 0), (3, 0),
    (3, 1), (3, 1),
    (3, 2), (3, 2),
    (3, 3)
  )

  // Count the outcome of a battle with a specific roll)
  private def applyRoll(attackerName: String, defenderName: String, clearing: ClearingState, roll: (Int, Int)): ClearingState =
    val result = clearing.clone
    val (highRoll, lowRoll) = roll
    val attacker = result.players(attackerName)
    val defender = result.players(defenderName)
    if defender.alliance then
      val damageToAttacker = min(highRoll, defender.warriors) + defender.modifier
      val damageToDefender = min(lowRoll, attacker.warriors) + attacker.modifier
      attacker.damage(damageToAttacker)
      defender.damage(damageToDefender)
    else
      val damageToAttacker = min(lowRoll, defender.warriors) + defender.modifier
      val damageToDefender = min(highRoll, attacker.warriors) + attacker.modifier
      attacker.damage(damageToAttacker)
      defender.damage(damageToDefender)
    result

  // All possible clearing states after an individual roll
  private def allOutcomesForRoll(attackerName: String, defenderName: String, clearing: ClearingState): Array[ClearingState] =
    allRolls.map(applyRoll(attackerName, defenderName, clearing, _))

  private def checkGoal(attackerName: String, defenderName: String, clearing: ClearingState, goal: String): Boolean =
    val attacker = clearing.players(attackerName)
    val defender = clearing.players(defenderName)
    goal match
      case "eliminate" => attacker.defenseless || defender.eliminated
      case "demolish" => attacker.defenseless || defender.eliminated || !clearing.isFull
      case "rule" => attacker.defenseless || defender.eliminated || clearing.ruler == attackerName
      case "build" => attacker.defenseless || defender.eliminated || (clearing.ruler == attackerName && !clearing.isFull)
      case "overthrow" => attacker.defenseless || clearing.ruler != defenderName
      case _ =>
        println("ERROR: Undefined goal for function checkGoal")
        false

  private def allOutcomesRepeated(attackerName: String, defenderName: String, clearing: ClearingState, goal: String, rollsLeft: Int): Array[ClearingState] =
    val attacker = clearing.players(attackerName)
    val defender = clearing.players(defenderName)
    if rollsLeft < 1 then Array(clearing)
    else if checkGoal(attackerName, defenderName, clearing, goal) then Array.fill(rollsLeft*16)(clearing.clone)
    else
      val outcomes = allOutcomesForRoll(attackerName, defenderName, clearing)
      var result: Array[ClearingState] = Array()
      outcomes.foreach(outcomeClearing => result = result ++ allOutcomesRepeated(attackerName, defenderName, outcomeClearing, goal, rollsLeft -1))
      result

  /**
   * Calculates the chance of the defender losing all of their pieces in at most 'rollCount' rolls.
   * Does not care whether the attacking players soldiers are also eliminated at the end.
   * @param attackerName alias for the attacking player
   * @param defenderName alias for the defending player
   * @param clearing the initial game state in the clearing (or the relevant information about it)
   * @param rollCount how many battles are to be fought
   */
  def eliminateChance(attackerName: String, defenderName: String, clearing: ClearingState, rollCount: Int): Unit =
    require(clearing.players.contains(attackerName) && clearing.players.contains(defenderName))
    val results = allOutcomesRepeated(attackerName, defenderName, clearing, "eliminate", rollCount)
    val percentage: Double = results.count(outcome =>
      outcome.players(defenderName).eliminated)
      / results.length.toDouble * 100
    println(s"The chance for $attackerName to remove all of $defenderName's pieces in $rollCount battle${if rollCount == 1 then "" else "s"} is ${twoDecimals.format(percentage)} %")

  /**
   * Calculates the chance of the attacker gaining rule over the clearing in at most 'rollCount' rolls. 
   * Does not care whether the attacking players soldiers are also eliminated at the end.
   * @param attackerName alias for the attacking player
   * @param defenderName alias for the defending player
   * @param clearing the initial game state in the clearing (or the relevant information about it)
   * @param rollCount how many battles are to be fought
   */
  def ruleChance(attackerName: String, defenderName: String, clearing: ClearingState, rollCount: Int): Unit =
    require(clearing.players.contains(attackerName) && clearing.players.contains(defenderName))
    val results = allOutcomesRepeated(attackerName, defenderName, clearing, "rule", rollCount)
    val percentage: Double = results.count(outcome =>
      outcome.ruler == attackerName)
      / results.length.toDouble * 100
    println(s"The chance for $attackerName to have control of the clearing after $rollCount battle${if rollCount == 1 then "" else "s"} is ${twoDecimals.format(percentage)} %")

  /**
   * Calculates the chance of the attacking player being able to build (requiring warriors, control, 
   * and a build slot) after at most 'rollCount' rolls. 
   * @param attackerName alias for the attacking player
   * @param defenderName alias for the defending player
   * @param clearing the initial game state in the clearing (or the relevant information about it)
   * @param rollCount how many battles are to be fought
   */
  def buildChance(attackerName: String, defenderName: String, clearing: ClearingState, rollCount: Int): Unit =
    require(clearing.players.contains(attackerName) && clearing.players.contains(defenderName))
    val results = allOutcomesRepeated(attackerName, defenderName, clearing, "build", rollCount)
    val percentage: Double = results.count(outcome =>
      outcome.ruler == attackerName && !outcome.isFull && !outcome.players(attackerName).defenseless)
      / results.length.toDouble * 100
    println(s"The chance for $attackerName to be able to build after $rollCount battle${if rollCount == 1 then "" else "s"} is ${twoDecimals.format(percentage)} %")

  /** 
   * Calculates the chance of the defender to lose rule over the clearing in at most 'rollCount' rolls.
   * Does not care whether the attacking players soldiers are also eliminated at the end.
   * @param attackerName alias for the attacking player
   * @param defenderName alias for the defending player
   * @param clearing the initial game state in the clearing (or the relevant information about it)
   * @param rollCount how many battles are to be fought
   */
  def overthrowChance(attackerName: String, defenderName: String, clearing: ClearingState, rollCount: Int): Unit =
    require(clearing.players.contains(attackerName) && clearing.players.contains(defenderName))
    val results = allOutcomesRepeated(attackerName, defenderName, clearing, "overthrow", rollCount)
    val percentage: Double = results.count(outcome =>
      outcome.ruler != defenderName)
      / results.length.toDouble * 100
    println(s"The chance for $defenderName to not control the clearing after $rollCount battle${if rollCount == 1 then "" else "s"} is ${twoDecimals.format(percentage)} %")
}
