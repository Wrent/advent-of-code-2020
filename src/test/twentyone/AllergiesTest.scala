package twentyone

import org.scalatest.funsuite.AnyFunSuite

class AllergiesTest extends AnyFunSuite {
  test("allergies") {
    assert(Allergies.countIngredients("mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\ntrh fvjkl sbzzf mxmxvkd (contains dairy)\nsqjhc fvjkl (contains soy)\nsqjhc mxmxvkd sbzzf (contains fish)") === 5)
  }

  test("allergies 2") {
    assert(Allergies.orderIngredients("mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\ntrh fvjkl sbzzf mxmxvkd (contains dairy)\nsqjhc fvjkl (contains soy)\nsqjhc mxmxvkd sbzzf (contains fish)") === "mxmxvkd,sqjhc,fvjkl")
  }

}
