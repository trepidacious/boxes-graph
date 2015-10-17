package org.rebeam.boxes.graph

import org.scalatest.WordSpec
import org.scalatest.Matchers._
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Arbitrary
import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

class IntervalSpec extends WordSpec with PropertyChecks with ShouldMatchers {

  val all = Interval.all

  def checkInfiniteAndNormalised(i: Interval) {
    i.isFinite shouldBe false
    i.isNormalised shouldBe true
    i.normalised.min shouldBe i.min
    i.normalised.max shouldBe i.max
  }

  val genSomeDouble = for {
    d <- arbitrary[Double]    
  } yield Some(d)

  val genOptionDouble = oneOf(const(None: Option[Double]), genSomeDouble)

  val genInterval = for {
    min <- genSomeDouble
    max <- genSomeDouble
  } yield Interval(min, max)

  implicit val arbitraryInterval = Arbitrary(genInterval)

  "Interval(None, None)" should {
    "contain all values" in {
      forAll{
        (d: Double) => all.contains(d) shouldBe true
      }
    }

    "be infinite and normalised" in {
      checkInfiniteAndNormalised(all)
    }

    "yield interval i.normalised from .finiteOuter(i)" in  {
      forAll{
        (i: Interval) => {
          val di = all.definedOuter(i) 
          val ni = i.normalised
          di.min shouldBe ni.min
          di.max shouldBe ni.max
        }
      }
    }

  }

  "Interval(Some(d), None)" should {

    "contain all values greater than or equal to d" in {
      forAll{
        (d: Double, v: Double) => {
          val i = Interval.greaterThanOrEqualTo(d)
          i.contains(d) shouldBe true
          i.contains(v) shouldBe v>= d
        }
      }
    }

    "be infinite and normalised" in {
      forAll{
        (d: Double) => {
          val i = Interval.greaterThanOrEqualTo(d)
          checkInfiniteAndNormalised(i)
        }
      }
    }
  }

  "Interval(None, Some(d))" should {

    "contain all values less than or equal to d" in {
      forAll{
        (d: Double, v: Double) => {
          val i = Interval.lessThanOrEqualTo(d)
          i.contains(d) shouldBe true
          i.contains(v) shouldBe v <= d
        }
      }
    }

    "be infinite and normalised" in {
      forAll{
        (d: Double) => {
          val i = Interval.lessThanOrEqualTo(d)
          checkInfiniteAndNormalised(i)
        }
      }
    }
  }

}