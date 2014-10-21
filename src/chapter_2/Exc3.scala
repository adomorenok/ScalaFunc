package chapter_2

import chapter_3.auxiliary._

object Exc3 extends App {

  def mkName(name: String): MyEither[String, Name] =
    if (name == "" || name == null) MyLeft("Name is empty.") else MyRight(new Name(name))

  def mkAge(age: Int): MyEither[String, Age] =
    if (age < 0) MyLeft("Age is out of range.") else MyRight(new Age(age))

  def mkPerson(name: String, age: Int): MyEither[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))

  println(mkPerson("John", 31))

}

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

