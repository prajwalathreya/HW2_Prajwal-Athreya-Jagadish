import prajwal.homework1_PAJ.hw1.*

import org.scalatest._
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import java.util.NoSuchElementException
//import matchers.should.Matchers._

class homework1_PAJtest extends featurespec.AnyFeatureSpec with GivenWhenThen {
  Feature("Sets Operation") {
    Scenario("newSet") {
      variables("set1", newSet("set1", Value(1), Value(2), Value(3))).evaluate()
      assert(Var("set1").evaluate() == Set(1, 2, 3))
    }
  }

  Feature("setUnion") {
    Scenario("setUnion") {
      variables("set2", newSet("set2", Value(4), Value(5), Value(6))).evaluate()
      assert(setUnion(Var("set1"), Var("set2")).evaluate() == Set(1, 2, 3, 4, 5, 6))

    }
  }
  Feature("setIntersection"){
    Scenario("setIntersection"){
      variables("set3",newSet("set3",Value(5),Value(8),Value(9))).evaluate()
      assert(setIntersection(Var("set2"),Var("set3")).evaluate() == Set(5))
    }
  }
  Feature("setdifference"){
    Scenario("setDifference"){
      variables("set4",newSet("set4",Value(8,9,10,11))).evaluate()
      assert(setDifference(Var("set2"),Var("set3")).evaluate() == Set(4,6))
    }

  }
  Feature("symmetric Difference"){
    Scenario("setSymmetricDifference"){
      variables("set5",newSet("set5",Value(9,10,11,12))).evaluate()
      assert(setSymmetricDifference(Var("set2"),Var("set3")).evaluate() == Set(4,6,8,9))
    }
  }
}






