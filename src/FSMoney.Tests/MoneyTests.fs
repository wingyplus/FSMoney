module FSMoney.Tests

open NUnit.Framework
open FSMoney.Money

[<Test>]
let TestCreate () =
    Assert.AreEqual({ Amount = 200; Currency = "USD" }, Money.create 200 "USD")


[<Test>]
let TestIsZero () =
    Money.create 0 "USD"
    |> Money.isZero
    |> Assert.True

    Money.create 200 "USD"
    |> Money.isZero
    |> Assert.False


[<Test>]
let TestIsPositive () =
    Money.create 1 "USD"
    |> Money.isPositive
    |> Assert.True

    Money.create 0 "USD"
    |> Money.isPositive
    |> Assert.False

    Money.create -1 "USD"
    |> Money.isPositive
    |> Assert.False


[<Test>]
let TestIsNegative () =
    Money.create 1 "USD"
    |> Money.isNegative
    |> Assert.False

    Money.create 0 "USD"
    |> Money.isNegative
    |> Assert.False

    Money.create -1 "USD"
    |> Money.isNegative
    |> Assert.True

[<Test>]
let TestEquals () =
    Money.equals (Money.create 20 "USD") (Money.create 20 "USD")
    |> Assert.True

    Money.equals (Money.create 30 "USD") (Money.create 20 "USD")
    |> Assert.False

    Money.equals (Money.create 20 "USD") (Money.create 20 "THB")
    |> Assert.False

[<Test>]
let TestCmp () =
    Assert.AreEqual(Eq, Money.cmp (Money.create 100 "USD") (Money.create 100 "USD"))
    Assert.AreEqual(Lt, Money.cmp (Money.create 10 "USD") (Money.create 100 "USD"))
    Assert.AreEqual(Gt, Money.cmp (Money.create 100 "USD") (Money.create 10 "USD"))

[<Test>]
let TestAdd () =
    Assert.AreEqual(Money.add (Money.create 20 "USD") 30, Money.add (Money.create 30 "USD") 20)
    Assert.AreEqual(Money.create 1075 "THB", Money.add (Money.create 1000 "THB") 0.75)

[<Test>]
let TestSubtract () =
    Assert.AreEqual(Money.create 10 "USD", Money.subtract (Money.create 20 "USD") 10)
    Assert.AreEqual(Money.create 1050 "THB", Money.subtract (Money.create 1100 "THB") 0.5)

[<Test>]
let TestMultiply () =
    Assert.AreEqual(Money.create 40 "USD", Money.multiply (Money.create 20 "USD") 2)
    Assert.AreEqual(Money.create 150 "USD", Money.multiply (Money.create 100 "USD") 1.5)
