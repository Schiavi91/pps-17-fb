package model

import utilities.Misc._

/** Defines the general implementation of a game character
  *
  * @author Nicola Atti
  */
trait Character {
  val characterName: String

  /** The player owning the character */
  val owner: Option[String]

  /** The character's fighting class */
  val role: String

  /** The character's statistics */
  val statistics: Statistics

  /** The character's statistic multipliers, specific to its role */
  val classMultipliers: ClassMultipliers

  /** The character's special moves */
  val specialMoves: Map[String, SpecialMove]

  /** Contains the character's status, depending on current HP and MP values and current modifiers and afflictions
    *
    */
  var status: Status = Status(
    healthPoints = calculateMaxHealthPoints(),
    manaPoints = calculateMaxManaPoints(),
    maxHealthPoints = calculateMaxHealthPoints(),
    maxManaPoints = calculateMaxManaPoints(),
    Map(),
    Map()
  )

  private def calculateMaxHealthPoints(): Int = {
    roundDown((this.statistics.resistance * this.classMultipliers.healthPoints) * 10)
  }

  private def calculateMaxManaPoints(): Int = {
    roundDown((this.statistics.spirit * this.classMultipliers.spirit) * 5)
  }

  import SubStatistic._

  /** Calculates the character's physical damage
    *
    * @return the character's physical damage
    */
  def physicalDamage: Int =
    roundDown(statistics.strength * classMultipliers.strength) + getModifierValues(PhysicalDamage)

  /** Calculates the character's bonus physical critical strike damage
    *
    * @return the character's physical critical strike damage
    */
  def physicalCriticalDamage: Int =
    roundDown((statistics.strength * classMultipliers.strength) / 5 + 150) + getModifierValues(PhysicalCriticalDamage)

  /** Calculates the character's speed
    *
    * @return the character's speed
    */
  def speed: Int =
    roundUp((statistics.agility * classMultipliers.agility) / 10) + getModifierValues(Speed)

  /** Calculates the character's critical strike chance
    *
    * @return the character's critical strike chance
    */
  def criticalChance: Int =
    roundDown((statistics.agility * classMultipliers.agility) / 2) + getModifierValues(CriticalChance)

  /** Calculates the character's magical defence
    *
    * @return the character's magical defence
    */
  def magicalDefence: Int =
    roundDown(statistics.spirit * classMultipliers.spirit) + getModifierValues(MagicalDefence)

  /** Calculates the character's magical power
    *
    * @return the character's magical power
    */
  def magicalPower: Int =
    roundDown(statistics.intelligence * classMultipliers.intelligence) + getModifierValues(MagicalDamage)

  /** Calculates the character's bonus magical critical strike power
    *
    * @return the character's magical critical strike power
    */
  def magicalCriticalPower: Int =
    roundDown((statistics.intelligence * classMultipliers.intelligence) / 10 + 150) + getModifierValues(
      MagicalCriticalDamage)

  /** Calculates the character's physical defence
    *
    * @return the character's physical defence
    */
  def physicalDefence: Int =
    roundDown(statistics.resistance * classMultipliers.resistance) + getModifierValues(PhysicalDefence)

  /** Check if the character is alive
    *
    * @return true if the character is alive of false if it's dead
    */
  def isAlive: Boolean = status.healthPoints > 0

  /** Check if the character is incapacitated
    *
    * @return true if the character is stunned or asleep
    */
  def isIncapacitated: Boolean =
    status.alterations.contains(Alteration.Stunned) || status.alterations.contains(Alteration.Asleep)

  /** Cycles all the character's modifiers for a given sub-statistic and calculates the bonus/malus value
    *
    * @param subStatistic the sub-statistic to search for
    * @return the total bonus/malus to the sub-statistic
    *
    */
  private def getModifierValues(subStatistic: SubStatistic): Int = {
    status.modifiers
      .filter { case (modifier, _) => modifier.affectsSubStatistic == subStatistic }
      .map { case (modifier, _) => modifier.delta }
      .sum
  }

}

import Character._

/** Defines a character of class Warrior
  *
  * @param characterName the character's name
  * @param owner the optional character's owner
  * @param statistics the character's statistics
  * @param specialMoves the character's special moves
  * @param classMultipliers the default statistic multipliers for the warrior fighting class
  * @param role the character's fighting class
  */
case class Warrior(characterName: String,
                   owner: Option[String],
                   statistics: Statistics,
                   specialMoves: Map[String, SpecialMove],
                   classMultipliers: ClassMultipliers = ClassMultipliers(2, 1, 1, 0.5, 1.5, 3),
                   role: String = WarriorRepresentation)
    extends Character {}

/** Defines a character of class Thief
  *
  * @param characterName the character's name
  * @param owner the optional character's owner
  * @param statistics the character's statistics
  * @param specialMoves the character's special moves
  * @param classMultipliers the default statistic multipliers for the thief fighting class
  * @param role the character's fighting class
  */
case class Thief(characterName: String,
                 owner: Option[String],
                 statistics: Statistics,
                 specialMoves: Map[String, SpecialMove],
                 classMultipliers: ClassMultipliers = ClassMultipliers(1.5, 2, 1, 0.5, 1, 2.5),
                 role: String = ThiefRepresentation)
    extends Character {}

/** Defines a character of class Wizard
  *
  * @param characterName the character's name
  * @param owner the optional character's owner
  * @param statistics the character's statistics
  * @param specialMoves the character's special moves
  * @param classMultipliers the default statistic multipliers for the wizard fighting class
  * @param role the character's fighting class
  */
case class Wizard(characterName: String,
                  owner: Option[String],
                  statistics: Statistics,
                  specialMoves: Map[String, SpecialMove],
                  classMultipliers: ClassMultipliers = ClassMultipliers(1, 1, 1.5, 2, 0.5, 2),
                  role: String = WizardRepresentation)
    extends Character {}

/** Defines a character of class Healer
  *
  * @param characterName the character's name
  * @param owner the optional character's owner
  * @param statistics the character's statistics
  * @param specialMoves the character's special moves
  * @param classMultipliers the default statistic multipliers for the healer fighting class
  * @param role the character's fighting class
  */
case class Healer(characterName: String,
                  owner: Option[String],
                  statistics: Statistics,
                  specialMoves: Map[String, SpecialMove],
                  classMultipliers: ClassMultipliers = ClassMultipliers(1.5, 0.5, 1, 2, 1, 2),
                  role: String = HealerRepresentation)
    extends Character {}

object Character {

  val WarriorRepresentation: String = "Warrior"
  val ThiefRepresentation: String = "Thief"
  val WizardRepresentation: String = "Wizard"
  val HealerRepresentation: String = "Healer"

  def apply(role: String,
            name: String,
            owner: Option[String],
            statistics: Statistics,
            specialMoves: Map[String, SpecialMove]): Character =
    role match {
      case WarriorRepresentation => Warrior(name, owner, statistics, specialMoves)
      case ThiefRepresentation   => Thief(name, owner, statistics, specialMoves)
      case WizardRepresentation  => Wizard(name, owner, statistics, specialMoves)
      case HealerRepresentation  => Healer(name, owner, statistics, specialMoves)
      case _                     => throw new IllegalArgumentException(s"Unknown character role: $role")
    }

}
