package example.lab2

/** Напишите отдельные функции, решающие поставленную задачу.
  * 
  * Синтаксис:
  *   // метод
  *   def myFunction(param0: Int, param1: String): Double = // тело
  * 
  *   // значение
  *   val myFunction: (Int, String) => Double (param0, param1) => // тело
  */
import example.lab2.Functions.{arearectangle, arearectangle2, calculateArea, testCircle, testRectangleCurried, testRectangleUc}

import scala.math._
object Functions {

  /* a) Напишите функцию, которая рассчитывает площадь окружности
   *    r^2 * Math.PI
   */
  def calculateArea(radius: Double): Double = Pi * pow(radius, 2.0)

  // примените вашу функцию из пункта (a) здесь, не изменяя сигнатуру
  def testCircle(r: Double): Double = calculateArea(r)


  /* b) Напишите карированную функцию которая рассчитывает площадь прямоугольника a * b.
   */
  def arearectangle(a:Double)(b:Double): Double = a*b
  //val multiply: (Double, Double) => Double = _ * _
  //val arearectangle: Double => Double => Double = multiply.curried

  // примените вашу функцию из пукта (b) здесь, не изменяя сигнатуру
  def testRectangleCurried(a: Double, b: Double): Double = arearectangle(a)(b)


  //c) Напишите не карированную функцию для расчета площади прямоугольника.
  def arearectangle2(a:Double,b:Double): Double ={
    a*b
  }


  // примените вашу функцию из пункта (c) здесь, не изменяя сигнатуру
  def testRectangleUc(a: Double, b: Double): Double = arearectangle2(a,b)
}
object start2 extends App{
  println(calculateArea(5))
  println(testCircle(5))
  println(arearectangle(2)(4))
  println(testRectangleCurried(2,4))
  println(arearectangle2(2,4))
  println(testRectangleUc(2,4))
}
