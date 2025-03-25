import scala.math._

/** Used to describe the state of a player on a clearing. 
 * Includes a method for taking damage, first removing warriors, then tokens, then buildings. 
 * If you want to prioritize buildings instead, simply leave tokens unmarked (for example if 
 * your goal is to remove a recruiter at Marquise's keep clearing). 
 * Additionally has methods for determining whether the player is defenseless and whether 
 * the player is eliminated.
 * 
 * @param _warriors the number of warriors the player has on a clearing
 * @param _buildings the number of buildings the player has on a clearing
 * @param _tokens the number of tokens the player has on a clearing
 * @param modifier positive if the player deals extra hits, negative if their opponent ignores hits
 * @param alliance true if the player's faction is the Woodland Alliance
 * @param dynasty true if the player's faction is the Eyrie Dynasties
 */
class PlayerState(private var _warriors: Int, 
              private var _buildings: Int, 
              private var _tokens: Int, 
              val modifier: Int = 0, 
              val alliance: Boolean = false, 
              val dynasty: Boolean = false) {
  def warriors = _warriors
  def buildings = _buildings
  def tokens = _tokens
  
  def defenseless: Boolean = _warriors == 0
  def eliminated: Boolean = _warriors + _tokens + _buildings == 0
  def damage(taken: Int): Unit =
    if defenseless then 
      _buildings = max(0, min(_buildings, _buildings - (taken+1 - _tokens)))
      _tokens = max(0, _tokens - (taken+1))
    else if taken <= _warriors then 
      _warriors -= taken
    else 
      val leftOver = taken - _warriors
      _warriors = 0
      _buildings = max(0, min(_buildings, _buildings - (leftOver - _tokens)))
      _tokens = max(0, _tokens - leftOver)
  
  override def clone: PlayerState = 
    PlayerState(warriors, buildings, tokens, modifier, alliance, dynasty)
  
  

}

