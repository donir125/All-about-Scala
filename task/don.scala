package example.lab1

object Hello extends Greeting with App {

  signum(14)
  fr()
  countdown(14)
  unicode("Hello")
  unicode2("Hello")
  sumsimplnum(111,120)
  sumnod(16)
  println(flatten[Int](List(List(1), List(List(2), 3), 4)),"19 zadanie")
  println(z21(List(1,2,3), 4),"21&23 zadanie")
  println(z24(44,33),"24 zadanie")
  println(zad17(2,2),"17 zadanie")
  PerfectNumber(7)
  sumpow(513)
  println(z25(List(1,4,5,2,3,5),2),"25z")
  println(z26(5,5),"z26")
  println(z27(List(1,2,3,4,5),2),"z27")
  println(z29(List(1,2,3,4,5,6,7)),"z29")
  println(z31(List(("один",1),("два",2),("три",3))))
}

trait Greeting {
  var check = false

  def signum(n:Int): Unit = {
      if (n>0){
        println(1)
        println("9----------------------")
      }else if(n<0) {
        println(-1)
        println("9----------------------")
      }else{
        println(0)
        println("9----------------------")
      }
  }

  def fr(): Unit ={
    println("11 zadanie")
    for(n <- 0 to 10){
      println(10-n)
    }
  }

  def countdown(n:Int): Unit ={
    if (n > 0){
      println("12----------------------")
      for(i <- 0 to n){
        println(n-i)
      }

    }else{
      println("12----------------------")
      for (i <- n to 0){
        print(i)
      }

    }
  }
  def unicode(n:String): Unit ={
    var string: BigInt = 1
    for (i <- n ){
      string = string * BigInt(i)
    }
    println("13----------------------")
    println(string)

  }
  def unicode2(n:String, text: BigInt = 1): Unit ={
      var a: Char = ' '
      if (n.length > 0){
        a = n.takeRight(1).charAt(0)
        unicode2(n.dropRight(1),text * BigInt(a))
      }else{
        println("14----------------------")
        println(text)

      }
  }

  def sumsimplnum(m: Int ,n:Int): Unit ={
      var result: Int = 0
      for (h <- m to n){
        if (h.toString.toSet.size == h.toString.length){
          result = result + h
        }
      }
      println("18----------------------------")
      println(result)
  }
  def sumnod(n: Int): Unit ={
    var result: BigInt = 0
    for(i <- 1 to n){
      var check = 0
      if (n%i==0){
        for (b <- 1 to i){
          if (i%b==0){
            check += 1
          }
        }
        if(check == 2){
          if (i > result){
            result = i
          }
        }
      }
    }
    println(result.toString.toList.map(x=>x.toInt - 48).sum, "20&22 zadanie")
  }
  import scala.reflect.ClassTag

  def flatten[T: ClassTag](list: List[Any]): List[T] =
    list.flatMap {
      case x: T => List(x)
      case sub: List[Any] => flatten[T](sub)
    }
  def zad17(x: Int, n: Int):Double = {
    var res:Double = 0
    if (n == 0) {res = 1}
    else if (n>0 & (n % 2 == 0)) {res = math.pow(zad17(x, n/2), 2)}
    else if (n>0 & (n % 2 == 1)) {res = x * zad17(x, n-1)}
    else {res = 1 / zad17(x, -1 *n)}
    res
  }
  def z21(lst: List[Any], k: Int): List[Any] = lst flatMap { n: Any => List.tabulate(k)(x => List(n)).flatten}


  def gcd(a: Int, b: Int): Int = b match {
    case 0 => a
    case n => gcd(b, a % b)
  }
  val z24: (Int, Int) => Int = (n: Int, m: Int) => n*m/gcd(n,m)

  def PerfectNumber (n:Int): Unit ={
    var count: Int = 0
    for(i <- 1 to n ){
      if ((n % i) == 0 && n != i){
        count += i
      }
    }
    if (count != n){
      PerfectNumber(n-1)
    }else{
      println(count,"28 zadanie")
    }
  }

  var z29 = 0
  def sumpow(n:Int): Unit = {
    var k = 0
    for(i <- 1 to n+1){
      k = n+1-i
      if (k.toString.toList.map(x=>x.toInt - 48).sum != 1){
        ggg(k.toString.toList.map(x=>x.toInt - 48).sum,2,k)
      }
    }
  }
  def ggg(n:Int,p:Int,n2:Int): Unit = {
    if (math.pow(n,p) <= n2){
      if (math.pow(n,p).toInt != n2){
        ggg(n,p+1,n2)
      }
      if(math.pow(n,p).toInt == n2){
        if(z29 < n2){
          z29 = n2
          println(z29,"30 zadanie")
        }
      }
    }
  }
  def z31(lst : List[(String, Int)]): List[Any] =  (lst.indices.collect { case i  => lst(i)._1 }).toList ::
    List((lst.indices.collect { case i  => lst(i)._2 }).toList)

  def z25(lst: List[Any], k: Int): List[Any] = (lst.indices.collect { case i if i % k != 0 => lst(i) }).toList

  def z26(n: Int, k: Int):Int ={
    if (k == 0)
      1
    else if (k > n)
      0
    else
      n * z26(n-1,k-1)
  }
  def z27(lst: List[Any], k: Int): List[Any] = {
    if(k>0)
      ((lst ::: lst.take(k)).takeRight(lst.length))
    else
      ((lst.takeRight(-1*k) ::: lst ).take(lst.length))
  }
  def z29(lst: List[Any]): List[Any] = (lst.indices.collect { case i if i % 2 != 0 => lst(i)}).toList ::
    List((lst.indices.collect { case j if j % 2 == 0 => lst(j)}).toList)
  /*def stepen(n: Int, s: Int): Int = {
    lazy val temps:Double = math.pow(n.toString.toList.map(x => x.toInt - 48).sum,s)
    if ( temps > n || temps == 1) 0 else if (temps == n ) s else stepen(n,s+1)
  }*/
}
