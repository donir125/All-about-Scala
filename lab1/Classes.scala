package example.lab2

/*
 * 
 a) Создать класс Animal, который имеет следующие поля:
 *      - name: String (название)
 *      - species: String (вид)
 *      - food: String
 * 
 *    Синтаксис: class MyClass(val publicField: Int, privateField: String) {
 *              // остальные поля и методы
 *            }
 * 
 * b) Создайте объект-компаньон для класса Animal и добавьте следующие сущности как поля:
 *      - cat, mammal, meat
 *      - parrot, bird, vegetables
 *      - goldfish, fish, plants
 * 
 *    Синтаксис: object MyClass {
 *              // статические поля и методы
 *            }
 * 
 * c) Добавьте следующие метод в Animals:
 *      def eats(food: String): Boolean
 *    
 *     который проверяет ест ли животное определенную пищу
 * 
 * d) Переопределите ваш класс Animal как трейт и создайте объекты класса-образца для Mammals, Birds и Fishs.
 *    Вам все еще нужно поле `species`?
 * 
 * e) Добавьте следующие функции в объект-компаньон Animal:
 *      def knownAnimal(name: String): Boolean  // true если это имя одного из трех животных из (b)
 *      def apply(name: String): Option[Animal] // возвращает одно из трех животных в соответствии с именем (Some) или ничего (None), см. ниже
 * 
 * f) Создайте трейт Food со следующими классами-образцами:
 *      - Meat
 *      - Vegetables
 *      - Plants
 *   и добавьте это в определение Animal. Так же добавьте объект-компаньон с методом apply():
 *      def apply(food: String): Option[Food]
 */
/*
class Animal(var name: String, var species:String,var food:String) {

  def eats(food: String):Boolean={
    if (this.food == food)true
    else false
  }

}
object Animal {
  val Cat = new Animal("cat", "mammal", "meat")
  val Parrot = new Animal("parrot","bird","vegetables")
  val Fish = new Animal("goldfish","fish","plants")

}
object start extends Greeting with App {
    val Cat = Animal.Cat
    val Parrot = Animal.Parrot
    val Fish = Animal.Fish
    println(Cat.eats("meat"))
}
*/
sealed trait Food {
  val name:String
}
case class Meat(name:String)       extends Food
case class Vegetables(name:String)    extends Food
case class Plants(name:String)       extends Food

sealed trait Animal extends Food{
  val name:String
  val food:Food
  def eats(food:String):Boolean={
    if (Foods.apply(food).isDefined) {
      Foods.apply(food).get.equals(this.food)
    }
    else false
  }
}



object Foods{
  val Beef = new Meat("Beef")
  val Pork = new Meat("Pork")
  val Cucumber = new Vegetables("Cucumber")
  val Carrot = new Vegetables("Carrot")
  val Plants = new Plants("Plants")

  def apply(name:String):Option[Food] = name match {
    case Pork.name => Some(Pork)
    case Beef.name => Some(Beef)
    case Cucumber.name => Some(Cucumber)
    case Carrot.name => Some(Carrot)
    case Plants.name => Some(Plants)
    case other => None
  }
}

object Animal {
  case class Mammal(name: String, food: Food) extends Animal
  case class Bird(name: String, food: Food) extends Animal
  case class Fish(name: String, food: Food) extends Animal

  val Cat: Mammal = Mammal("cat", Foods.Pork)
  val Parrot:Bird = Bird("parrot",Foods.Carrot)
  val goldenFish:Fish = Fish("goldfish",Foods.Plants)

  def knownAnimal(name: String): Boolean = name.equals(Cat.name) || name.equals(Parrot.name) || name.equals(goldenFish.name)

  def apply(name: String): Option[Animal] = name match {
      case Cat.name => Some(Cat)
      case Parrot.name => Some(Parrot)
      case goldenFish.name => Some(goldenFish)
      case none => None
  }
}
object  start extends App{
    val Cat = Animal.Cat
    val Parrot = Animal.Parrot
    val Fish = Animal.goldenFish
    val Human = Animal.Mammal("Human",Foods.Beef)
    println(Cat.eats("Pork"))
    println(Cat.eats("Plants"))

    println(Human.eats("Beef"))
    println(Human.eats("Plantsss"))
}
/*
sealed trait Option[A] {

  def isEmpty: Boolean
}
case class Some[A](a: A) extends Option[A] {
  val isEmpty = false
}
case class None[A]()     extends Option[A] {
  val isEmpty = true
}
*/
