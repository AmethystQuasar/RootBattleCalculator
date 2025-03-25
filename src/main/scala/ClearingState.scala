import scala.math._


/** Used to describe the state of a clearing. Includes methods for telling whether
 * there are available slots to build in and who rules the clearing.
 * 
 * @param buildingSlots the number of slots available for players to build in (discounting ruin tiles)
 * @param players a map of player names to their respective states on the clearing
 */
class ClearingState(val buildingSlots: Int, val players: Map[String, PlayerState])  {
  
  def isFull: Boolean = players.values.map(_.buildings).sum >= buildingSlots

  def ruler: String =
    var record: Double = 0
    var result = "empty"
      for (name, player) <- players do
        val ruleSum = player.buildings + player.warriors
        if ruleSum == record && player.dynasty then 
          result = name
          record += 0.5
        else if ruleSum == record && record > 0 then
          result = "tied"
        else if ruleSum > record then 
          result = name
          record = ruleSum
    result
  
  private def clonePlayers: Map[String, PlayerState] = 
    players.map((name, player) => name -> player.clone)
  override def clone: ClearingState = ClearingState(buildingSlots, clonePlayers)
  
}
