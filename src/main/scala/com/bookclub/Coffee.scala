package com.bookclub

case class Coffee(price: Double = 5.0)

case class CreditCard(name: String = "Default Card") {
  def charge(price: Double): Unit = println(s"You made a payment of $price")
}

trait Payments {
  def charge(cc: CreditCard, price: Double): Unit
}

trait Foo {
  def foo(): Unit
}

class PaymentsImpl extends Payments {
  override def charge(cc: CreditCard, price: Double): Unit = println(s"You made a payment of $price with credit card: ${cc.name}")
}

class PaymentsImplTwo extends Payments with Foo {
  override def charge(cc: CreditCard, price: Double): Unit = println(s"This message sucks because it gives you no information")
  def foo() = println("bar")
}

case class Charge(cc: CreditCard, amount: Double) {
  def combine(other: Charge): Charge = {
    if (cc == other.cc)
      Charge(cc, amount + other.amount)
    else
      throw new Exception(s"Dummy, that's different credit cards, $cc and ${other.cc}")
  }
}

class Cafe {
  def buyCoffeeWith(cc: CreditCard): Coffee = {
    val cup = new Coffee()

    cc.charge(cup.price) // side-effect (it's doing something but the caller of the function has no idea about it)

    cup
  }

  def buyCoffee(cc: CreditCard, p: Payments): Coffee = {
    val cup = new Coffee()

    p.charge(cc, cup.price) // still a side-effect but a little better

    cup
  }

  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee()

    // this returns the coffee and the payment info
    // but notice we're not actually making the payment here
    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, numCoffees: Int): (Array[Coffee], Charge) = {
    val purchases: Array[(Coffee, Charge)] = Array.fill(numCoffees)(buyCoffee(cc)) // get coffee + payment info for N coffees
    val (coffees, charges): (Array[Coffee], Array[Charge]) = purchases.unzip // separate coffees + payment info pairs

    (coffees, charges.reduce((c1,c2) => c1.combine(c2))) // add up all the separate payments for the coffees into one big payment
  }

  def buyCoffeesTwo(cc: CreditCard, numCoffees: Int): (Array[Coffee], Charge) = {
    val purchases: Array[(Coffee, Charge)] =
      (1 to numCoffees)
        .toArray
        .map(_ => buyCoffee(cc))
    val (coffees, charges): (Array[Coffee], Array[Charge]) = purchases.unzip

    (coffees, charges.reduce((c1,c2) => c1.combine(c2)))
  }
}

object TestCoffee extends App {
  val cafe = new Cafe

  cafe.buyCoffeeWith(CreditCard())
  cafe.buyCoffee(CreditCard(), new PaymentsImpl)
  cafe.buyCoffee(CreditCard(), new PaymentsImplTwo)
  (new PaymentsImplTwo).foo()
  val result = cafe.buyCoffee(CreditCard()) // notice this doesn't print anything
  println(result)
  println(cafe.buyCoffees(CreditCard(), 3))
  println(Charge(CreditCard(), 5).combine(Charge(CreditCard(), 7.0)))
  println(Charge(CreditCard(), 5).combine(Charge(CreditCard("New Card"), 7.0))) // this will throw an exception
}

object PureFunctions extends App {
  val x = new StringBuilder("hello")
  println(x)
  val y = x.append(" world")
  println(y)
  val z = x.append(" world")
  println(z)
}
