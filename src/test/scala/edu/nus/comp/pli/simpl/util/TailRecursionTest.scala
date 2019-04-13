package edu.nus.comp.pli.simpl.util

import edu.nus.comp.pli.simpl.parser.SimplParser._
import edu.nus.comp.pli.simpl.util.SimplEplDenotationalSemantics._
import edu.nus.comp.pli.simpl.util.SimplEvmInstruction._
import edu.nus.comp.pli.simpl.util.TypeInfer._
import org.scalatest.FlatSpec

import scala.io.Source

class TailRecursionTest extends FlatSpec {
  "test31.simpl" should "have tailcall optimisation" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test31.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) =>
          compile(parse(source)) ==
            List(LDCI(1), LDFR(List(),("f",0),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LD(("n",1)), LD(("f",0)), TAILCALL(1))
      }
    )
  }
  "test32.simpl" should "have not have tailcall optimisation" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test32.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) =>
          println(compile(parse(source)))
          compile(parse(source)) ==
            List(LDCB(true), JOF("labal_0"), LDCI(1), LDF(Vector(),1,"labal_2"), CALL(1), GOTO("labal_1"), LABEL("labal_0"), LDCI(1), LDF(Vector(),1,"labal_3"), CALL(1), LABEL("labal_1"), DONE, LABEL("labal_2"), LD(("x",0)), RTN, LABEL("labal_3"), LD(("x",0)), LDCI(1), PLUS, RTN)
      }
    )
  }
  "test33.simpl" should "have have tailcall optimisation" in {
    // an if-else case might have an opportunity for tailcall optimisation, if the branches are function applications
    // the current tail_call_op includes tailcall optimisation for the else (but not the if) case, by searching for
    // CALL(n) :: LABEL(x) :: RTN
    val source = Source.fromURL(getClass.getResource("/simpl/test33.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) =>
          println(compile(parse(source)))
          compile(parse(source)) ==
            List(LDFR(List(),("factorial",0),1,"labal_0"), DONE, LABEL("labal_0"), LD(("n",1)), LDCI(0), EQ, JOF("labal_1"), LDCI(1), GOTO("labal_2"), LABEL("labal_1"), LD(("n",1)), LDCI(1), MINUS, LD(("factorial",0)), TAILCALL(1), LABEL("labal_2"), RTN)
      }
    )
  }
  "test04.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test04.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) =>
          compile(parse(source)) ==
            List(LDFR(List(),("recurse",0),4,"labal_0"), DONE, LABEL("labal_0"), LD(("y",2)), LDCI(0), EQ, JOF("labal_1"), LD(("iv",4)), GOTO("labal_2"), LABEL("labal_1"), LD(("iv",4)), LD(("op",3)), LD(("y",2)), LDCI(1), MINUS, LD(("x",1)), LD(("recurse",0)), CALL(4), LD(("x",1)), LD(("op",3)), TAILCALL(2), LABEL("labal_2"), RTN)
      }
    )
  }
  "test07.simpl" should "have tailcall optimisation" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test07.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDF(Vector(),2,"labal_3"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDF(Vector(),2,"labal_2"), LDF(Vector(("f",0)),1,"labal_1"), TAILCALL(1), LABEL("labal_1"), LDCI(4), LDCI(3), LD(("f",0)), CALL(1), LD(("apply",1)), TAILCALL(2), LABEL("labal_2"), LD(("x",1)), LD(("g",0)), TAILCALL(1), LABEL("labal_3"), LD(("x",0)), LD(("y",1)), PLUS, RTN)

      }
    )
  }
  "test08.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test08.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDFR(Vector(),("mypower",0),2,"labal_1"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(3), LDCI(17), LD(("power",0)), TAILCALL(2), LABEL("labal_1"), LD(("y",2)), LDCI(0), EQ, JOF("labal_2"), LDCI(1), GOTO("labal_3"), LABEL("labal_2"), LD(("x",1)), LD(("y",2)), LDCI(1), MINUS, LD(("x",1)), LD(("mypower",0)), CALL(2), TIMES, LABEL("labal_3"), RTN)

      }
    )
  }
  "test09.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test09.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDFR(List(),("recurse",0),4,"labal_2"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(1), LDF(Vector(),2,"labal_1"), LDCI(3), LDCI(2), LD(("recurse",0)), TAILCALL(4), LABEL("labal_1"), LD(("x",0)), LD(("z",1)), TIMES, RTN, LABEL("labal_2"), LD(("y",2)), LDCI(0), EQ, JOF("labal_3"), LD(("initvalue",4)), GOTO("labal_4"), LABEL("labal_3"), LD(("initvalue",4)), LD(("operation",3)), LD(("y",2)), LDCI(1), MINUS, LD(("x",1)), LD(("recurse",0)), CALL(4), LD(("x",1)), LD(("operation",3)), TAILCALL(2), LABEL("labal_4"), RTN)

      }
    )
  }

  "test10.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test10.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDFR(List(),("recurse",0),4,"labal_2"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(0), LDF(Vector(),2,"labal_1"), LDCI(3), LDCI(2), LD(("recurse",0)), TAILCALL(4), LABEL("labal_1"), LD(("x",0)), LD(("z",1)), PLUS, RTN, LABEL("labal_2"), LD(("y",2)), LDCI(0), EQ, JOF("labal_3"), LD(("initvalue",4)), GOTO("labal_4"), LABEL("labal_3"), LD(("initvalue",4)), LD(("operation",3)), LD(("y",2)), LDCI(1), MINUS, LD(("x",1)), LD(("recurse",0)), CALL(4), LD(("x",1)), LD(("operation",3)), TAILCALL(2), LABEL("labal_4"), RTN)

      }
    )
  }
  "test11.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test11.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDFR(List(),("recurse",0),4,"labal_2"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(128), LDF(Vector(),2,"labal_1"), LDCI(3), LDCI(2), LD(("recurse",0)), TAILCALL(4), LABEL("labal_1"), LD(("z",1)), LD(("x",0)), DIV, RTN, LABEL("labal_2"), LD(("y",2)), LDCI(0), EQ, JOF("labal_3"), LD(("initvalue",4)), GOTO("labal_4"), LABEL("labal_3"), LD(("initvalue",4)), LD(("operation",3)), LD(("y",2)), LDCI(1), MINUS, LD(("x",1)), LD(("recurse",0)), CALL(4), LD(("x",1)), LD(("operation",3)), TAILCALL(2), LABEL("labal_4"), RTN)

      }
    )
  }


  "test12.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test12.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDF(Vector(),1,"labal_2"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(3), LDF(Vector(("square",0)),1,"labal_1"), TAILCALL(1), LABEL("labal_1"), LDCI(4), LD(("x",1)), LD(("square",0)), CALL(1), TIMES, RTN, LABEL("labal_2"), LD(("x",0)), LD(("x",0)), TIMES, RTN)

      }
    )
  }
  "test13.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test13.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDCI(5), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDF(Vector(("s",0)),1,"labal_3"), LDF(Vector(),1,"labal_1"), TAILCALL(1), LABEL("labal_1"), LDCI(10), LDF(Vector(("f",0)),1,"labal_2"), TAILCALL(1), LABEL("labal_2"), LDCI(3), LD(("f",0)), TAILCALL(1), LABEL("labal_3"), LD(("x",1)), LD(("s",0)), PLUS, RTN)

      }
    )
  }
  "test16.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test16.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDCI(3), LDFR(Vector(),("f",0),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LD(("n",1)), LD(("f",0)), TAILCALL(1))

      }
    )
  }
  "test17.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test17.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDCI(3), LDF(Vector(),1,"labal_1"), LDF(Vector(),2,"labal_0"), CALL(2), DONE, LABEL("labal_0"), LD(("y",1)), LD(("x",0)), TAILCALL(1), LABEL("labal_1"), LD(("x",0)), LDCI(1), PLUS, RTN)

      }
    )
  }
  "test19.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test19.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDFR(Vector(),("facloop",0),2,"labal_3"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDF(Vector(("facloop",0)),1,"labal_2"), LDF(Vector(),1,"labal_1"), TAILCALL(1), LABEL("labal_1"), LDCI(4), LD(("fac",0)), TAILCALL(1), LABEL("labal_2"), LDCI(1), LD(("n",1)), LD(("facloop",0)), TAILCALL(2), LABEL("labal_3"), LD(("n",1)), LDCI(1), EQ, JOF("labal_4"), LD(("acc",2)), GOTO("labal_5"), LABEL("labal_4"), LD(("acc",2)), LD(("n",1)), TIMES, LD(("n",1)), LDCI(1), MINUS, LD(("facloop",0)), TAILCALL(2), LABEL("labal_5"), RTN)

      }
    )
  }
  "test20.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test20.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDF(Vector(),1,"labal_1"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(3), LD(("fac",0)), TAILCALL(1), LABEL("labal_1"), LD(("n",0)), LDCI(1), PLUS, RTN)

      }
    )
  }
  "test21.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test21.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDFR(Vector(),("foo",0),1,"labal_1"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(4), LD(("f",0)), TAILCALL(1), LABEL("labal_1"), LD(("n",1)), LDCI(0), EQ, JOF("labal_2"), LDCI(0), GOTO("labal_3"), LABEL("labal_2"), LDCI(2), LD(("n",1)), LDCI(1), MINUS, LD(("foo",0)), CALL(1), PLUS, LABEL("labal_3"), RTN)

      }
    )
  }
  "test22.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test22.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDFR(Vector(),("foo",0),1,"labal_1"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(4), LD(("f",0)), TAILCALL(1), LABEL("labal_1"), LD(("n",1)), LDCI(0), EQ, JOF("labal_2"), LDCI(0), GOTO("labal_3"), LABEL("labal_2"), LD(("n",1)), LDCI(1), MINUS, LD(("foo",0)), CALL(1), LD(("n",1)), PLUS, LABEL("labal_3"), RTN)

      }
    )
  }
  "test23.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test23.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDFR(Vector(),("foo",0),1,"labal_1"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(1), LDCI(30), LD(("f",0)), TAILCALL(2), LABEL("labal_1"), LD(("n",1)), LDCI(0), EQ, JOF("labal_2"), LDF(Vector(),1,"labal_4"), GOTO("labal_3"), LABEL("labal_2"), LDF(Vector(("foo",0), ("n",1)),1,"labal_5"), LABEL("labal_3"), RTN, LABEL("labal_4"), LD(("acc",0)), RTN, LABEL("labal_5"), LD(("acc",2)), LD(("n",1)), TIMES, LD(("n",1)), LDCI(1), MINUS, LD(("foo",0)), TAILCALL(2))

      }
    )
  }
  "test24.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test24.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDCI(3), LDF(Vector(),1,"labal_1"), CALL(1), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(4), LD(("f",0)), TAILCALL(1), LABEL("labal_1"), LDF(Vector(("x",0)),1,"labal_2"), RTN, LABEL("labal_2"), LD(("x",0)), LD(("y",1)), PLUS, RTN)

      }
    )
  }
  "test25.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test25.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDCI(3), LDF(Vector(),1,"labal_1"), CALL(1), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(4), LD(("f",0)), TAILCALL(1), LABEL("labal_1"), LDF(Vector(("x",0)),1,"labal_2"), RTN, LABEL("labal_2"), LD(("x",0)), LD(("y",1)), PLUS, RTN)

      }
    )
  }
  "test26.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test26.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDCI(3), LDF(Vector(),1,"labal_1"), CALL(1), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(4), LD(("f",0)), TAILCALL(1), LABEL("labal_1"), LDF(Vector(("x",0)),1,"labal_2"), RTN, LABEL("labal_2"), LD(("x",0)), LD(("y",1)), PLUS, RTN)

      }
    )
  }
  "test27.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test27.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDF(Vector(),1,"labal_1"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(4), LD(("f",0)), TAILCALL(1), LABEL("labal_1"), LDF(Vector(("x",0)),1,"labal_2"), RTN, LABEL("labal_2"), LD(("x",0)), LD(("y",1)), PLUS, RTN)

      }
    )
  }
  "test28.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test28.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) == List(LDF(Vector(),1,"labal_2"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDF(Vector(("f",0)),1,"labal_1"), RTN, LABEL("labal_1"), LD(("pa",1)), LDCI(4), LD(("f",0)), TAILCALL(2), LABEL("labal_2"), LDF(Vector(("x",0)),1,"labal_3"), RTN, LABEL("labal_3"), LD(("x",0)), LD(("y",1)), PLUS, RTN)

      }
    )
  }
  "test29.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test29.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) == List(LDF(Vector(),1,"labal_3"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(3), LDF(Vector(("f",0)),1,"labal_2"), CALL(1), LDF(Vector(),1,"labal_1"), TAILCALL(1), LABEL("labal_1"), LDCI(4), LD(("g",0)), TAILCALL(1), LABEL("labal_2"), LDCI(3), LD(("f",0)), TAILCALL(1), LABEL("labal_3"), LDF(Vector(("x",0)),1,"labal_4"), RTN, LABEL("labal_4"), LD(("x",0)), LD(("y",1)), PLUS, RTN)

      }
    )
  }

}
