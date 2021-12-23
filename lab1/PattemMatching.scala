package example.lab2

/** Напишите решение в виде функции.
  * 
  * Синтаксис:
  *   val a: Int = ???
  * 
  *   a match {
  *     case 0 => true
  *     case _ => false
  *   }
  */
object PatternMatching extends App{

  sealed trait Hand
  case object Rock    extends Hand
  case object Paper   extends Hand
  case object Scissor extends Hand

  sealed trait Result
  case object Win  extends Result
  case object Lose extends Result
  case object Draw extends Result

  sealed trait Food
  case object Meat       extends Food
  case object Vegetables extends Food
  case object Plants     extends Food

  sealed trait Animal {

    val name: String
    var food: Food
  }
  case class Mammal(name: String, var food: Food, weight: Int) extends Animal
  case class Fish(name: String, var food: Food)                extends Animal
  case class Bird(name: String, var food: Food)                extends Animal



  /* a) Напишите функцию, которая ставит в соответствие числу строку слудеющим образом:
   * Если:
   *     1 => "it is one"
   *     2 => "it is two"
   *     3 => "it is three"
   *     иначе => "what's that"
   */
  def numtosring(value:Int): String ={
    value match {
      case 1 => "it is one"
      case 2 => "it is two"
      case 3 => "it is three"
      case _ => "what's that"
    }
  }
  // примените вашу функцию из пункта (a) здесь, не изменяя сигнатуру
  def testIntToString(value: Int): String = numtosring(1)

  /* b) Напишите функцию которая возвращает true если переменная `value` принимает значение:
   *     "max" или "Max"
   *     "moritz" или "Moritz"
   */

  def mmMM(value: String): Boolean ={
    if (value == "max" || value == "Max" || value == "moritz" || value == "Moritz") true
    else false
  }

  // примените функции из пункта (b) здесь, не изменяя сигнатуру
  def testIsMaxAndMoritz(value: String): Boolean = mmMM(value)

  // c) Напишите функцию проверки является ли `value` четным 
  def numcheck(n:Int):Boolean={
    if(n%2==0){
      true
    }else false
  }


  // примените функции из пункта (c) здесь, не изменяя сигнатуру
  def testIsEven(value: Int): Boolean = numcheck(value)


  
  /* d) Напишите функцию, моделирующую игру в Камень ножницы бумага 
   *     1. камень побеждает ножницы
   *     2. ножницы побеждают бумагу
   *     3. бумага побеждает камень
   *    Выиграет ли игрок `a`?
   */
  def RockPaperScissors(a:Hand,b:Hand):Result= {
    a match {
      case Rock => b match {
        case Rock => Draw
        case Paper => Lose
        case Scissor => Win
      }
      case Paper => b match {
        case Rock => Win
        case Paper => Draw
        case Scissor => Lose
      }
      case Scissor => b match {
        case Rock => Lose
        case Paper => Win
        case Scissor => Draw
      }
    }
  }

  // примените вашу функцию из пункта (d) здесь, не изменяя сигнатуру
  def testWinsA(a: Hand, b: Hand): Result = RockPaperScissors(a,b)



  // Примечание: используйте определение Animals

  // e) Верните вес (weight: Int) объекта Mammal, иначе верните -1.

    def MammalWeight(animal:Animal):Int={
      animal match {
        case mammal: Mammal => mammal.weight
        case dif => -1
      }
    }



  // примените функцию из пункта (e) здесь, не изменяйте сигнатуру
  def testExtractMammalWeight(animal: Animal): Int = MammalWeight(animal)


  // f) Измените поле еда объектов классов Fishes и Birds на Plants, класс Mammels оставьте неизмененным.
  def changeFood(animal: Animal): Animal =
    animal match {
      case fish: Fish => fish.food = Plants;animal
      case bird: Bird => bird.food = Plants;animal
      case other => animal
    }


  // примените функцию из пункта (f) здесь, не изменяйте сигнатуру
  def testUpdateFood(animal: Animal): Animal = changeFood(animal)

  println(testIntToString(1))
  println(testIsMaxAndMoritz("max"))
  println(testIsEven(4))
  println(testWinsA(Rock,Paper))
  println(testExtractMammalWeight(Mammal("Kitty", Meat, 7)))
  println(testUpdateFood(Fish("ribka", Meat)))
  println(testUpdateFood(Mammal("Kitty", Meat, 7)))



}

