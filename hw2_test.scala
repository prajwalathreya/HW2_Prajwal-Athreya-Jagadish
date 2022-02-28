import homework2.hw2.*
import org.scalatest.{GivenWhenThen, featurespec}
import prajwal.homework1_PAJ.hw1.{Value, Var, newSet, setDifference, setIntersection, setSymmetricDifference, setUnion, variables}

class homework1_PAJtest extends featurespec.AnyFeatureSpec with GivenWhenThen {
  Feature("class constructs") {
    Scenario("ourClass") {
      variables("set1", newSet("set1", Value(1), Value(2), Value(3))).evaluate()
      assert(Var("set1").evaluate() == Set(1, 2, 3))
    }
  }

  Feature("class construct") {
    Scenario("ourInstanceVariable") {
      variables("set2", newSet("set2", Value(4), Value(5), Value(6))).evaluate()
      assert(setUnion(Var("set1"), Var("set2")).evaluate() == Set(1, 2, 3, 4, 5, 6))

    }
  }
  Feature("class construct"){
    Scenario("ourMethod"){
      variables("set3",newSet("set3",Value(5),Value(8),Value(9))).evaluate()
      assert(setIntersection(Var("set2"),Var("set3")).evaluate() == Set(5))
    }
  }
  Feature("class construct"){
    Scenario("ourConstructor"){
      variables("set4",newSet("set4",Value(8,9,10,11))).evaluate()
      assert(setDifference(Var("set2"),Var("set3")).evaluate() == Set(4,6))
    }

  }
  Feature("class construct"){
    Scenario("invokeMethod"){
      variables("set5",newSet("set5",Value(9,10,11,12))).evaluate()
      assert(setSymmetricDifference(Var("set2"),Var("set3")).evaluate() == Set(4,6,8,9))
    }
  }
}
