package adpro

import java.util.concurrent.{Callable, ExecutorService}

import org.scalatest.FunSuite
import Par._
import org.scalamock.scalatest.MockFactory

class Par_test extends FunSuite with MockFactory {

  private def executorMock[T]: ExecutorService = {
    val esMock = stub[ExecutorService]
    (esMock.submit(_: Callable[T])).when(*).onCall((c: Callable[T]) => UnitFuture(c.call()))
    esMock
  }

  test("This one always works: (-1) * (-1) = 1") {
    assert((-1) * (-1) == 1)
  }

  test("asyncF is lazy") {
    asyncF((i: Int) => {
      fail()
      i * i
    })(33)
  }

  test("asyncF computes correct result") {
    val par = asyncF((i: Int) => i * i)(33)
    val res = par(executorMock).get()

    assert(res === 1089)
  }

  test("sequence returns expected result") {
    val es = executorMock
    val input = List[Par[Int]](Par.lazyUnit(3), Par.fork(Par.fork(Par.unit(12))), Par.unit(9), Par.lazyUnit(8))

    val expectedOutput = List(3, 12, 9, 8)

    val output = Par.sequence(input)
    val outputRes = output(es)

    assert(outputRes.get() === expectedOutput)
  }

  test("parMap does what I expect it does") {
    val es = executorMock

    val input = List[Int](33, 12, 45, 0, 2, 13, 13)

    val expectedOutputRes = List[String]("33", "12", "45", "0", "2", "13", "13")

    val output = Par.parMap(input)(_.toString)
    val outputRes = output(es).get()

    assert(expectedOutputRes === outputRes)
  }

  test("parFilter returns expected result") {
    val es = executorMock
    val input = List[Int](33, 12, 45, 0, 2, 13, 13, 13, 22, 22, 6, 22, 9, 50)
    val expectedOutputRes = List[Int](33, 45, 22, 22, 22, 50)

    val output = Par.parFilter(input)(_ > 20)
    val outputRes = output(es).get()

    assert(outputRes === expectedOutputRes)
  }

  test("parFilter is lazy") {
    val input = List[Int](33, 12, 45, 0, 2, 13, 13, 13, 22, 22, 6, 22, 9, 50)
    val crashFunc: Int => Boolean = _ => fail()
    Par.parFilter(input)(crashFunc)
  }

  test("map3 returns expected result") {
    val es = executorMock

    val input1 = Par.lazyUnit(34)
    val input2 = Par.unit(78)
    val input3 = Par.lazyUnit((a: Int) => a * 100)

    val output = Par.map3(input1, input2, input3)((a, b, c) => c(a + b).toString)

    val outputRes = output(es).get()

    assert(outputRes === "11200")
  }

  test("map3 is lazy") {
    val es = executorMock

    val input1 = Par.lazyUnit(34)
    val input2 = Par.unit(78)
    val input3 = Par.lazyUnit(Par.asyncF((a: Int) => a * 100))

    Par.map3(input1, input2, input3)((a, b, c) => {
      fail()
      c(a + b).toString()
    })
  }

  test("choice takes left result if true") {
    val es = executorMock
    val left = Par.lazyUnit(45)
    val right = Par.lazyUnit(120)

    val condTrue = Par.lazyUnit(true)

    val result = choice(condTrue)(left, right)(es).get

    assert(result === 45)
  }

  test("choice takes right if result false") {
    val es = executorMock
    val left = Par.lazyUnit(45)
    val right = Par.lazyUnit(120)

    val condFalse = Par.lazyUnit(false)

    val result = choice(condFalse)(left, right)(es).get

    assert(result === 120)
  }

  test("choice is lazy") {
    val es = executorMock
    val left = Par.lazyUnit(45)
    val right = Par.lazyUnit(120)

    val cond = Par.lazyUnit({
      fail()
      false
    })

    choice(cond)(left, right)
  }

  test("choiceN chooses nth element") {
    val es = executorMock
    val choices = List[Par[Int]](lazyUnit(33), unit(12), lazyUnit(55), lazyUnit(99), unit(0))

    val cond = lazyUnit(3)

    val result = Par.choiceN(cond)(choices)(es).get

    assert(result === 99)
  }

  test("choiceN makes choice lazily") {
    val choices = List[Par[Int]](lazyUnit(33), unit(12), lazyUnit(55), lazyUnit(99), unit(0))

    val cond = lazyUnit({
      fail()
      2
    })

    Par.choiceN(cond)(choices)
  }



  test("choiceViaChooser takes left result if true") {
    val es = executorMock
    val left = Par.lazyUnit(45)
    val right = Par.lazyUnit(120)

    val condTrue = Par.lazyUnit(true)

    val result = choiceViaChooser(condTrue)(left, right)(es).get

    assert(result === 45)
  }

  test("choiceViaChooser takes right if result false") {
    val es = executorMock
    val left = Par.lazyUnit(45)
    val right = Par.lazyUnit(120)

    val condFalse = Par.lazyUnit(false)

    val result = choiceViaChooser(condFalse)(left, right)(es).get

    assert(result === 120)
  }

  test("choiceViaChooser is lazy") {
    val es = executorMock
    val left = Par.lazyUnit(45)
    val right = Par.lazyUnit(120)

    val cond = Par.lazyUnit({
      fail()
      false
    })

    choiceViaChooser(cond)(left, right)
  }

  test("choiceNViaChooser chooses nth element") {
    val es = executorMock
    val choices = List[Par[Int]](lazyUnit(33), unit(12), lazyUnit(55), lazyUnit(99), unit(0))

    val cond = lazyUnit(3)

    val result = Par.choiceNviaChooser(cond)(choices)(es).get

    assert(result === 99)
  }

  test("choiceNViaChooser makes choice lazily") {
    val choices = List[Par[Int]](lazyUnit(33), unit(12), lazyUnit(55), lazyUnit(99), unit(0))

    val cond = lazyUnit({
      fail()
      2
    })

    Par.choiceNviaChooser(cond)(choices)
  }

  test("join joins two parallel computations"){
    val es = executorMock
    val nestedComp = lazyUnit(lazyUnit(200))

    val expectedRes = lazyUnit(200)

    val res = join(nestedComp)

    assert(Par.equal(es)(expectedRes, res))
  }

}