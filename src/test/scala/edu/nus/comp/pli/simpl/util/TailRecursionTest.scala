package edu.nus.comp.pli.simpl.util

import edu.nus.comp.pli.simpl.parser.SimplParser._
import edu.nus.comp.pli.simpl.util.SimplEplDenotationalSemantics._
import edu.nus.comp.pli.simpl.util.SimplEvmInstruction._
import edu.nus.comp.pli.simpl.util.TypeInfer._
import org.scalatest.FlatSpec

import scala.io.Source

class TailRecursionTest extends FlatSpec {
  "test04.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test04.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==
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
        case Some (t) => compile(parse(source)) ==List(LDFR(List(),("recurse",0),4,"labal_2"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(1), LDF(Vector(),2,"labal_1"), LDCI(3), LDCI(2), LD(("recurse",0)), TAILCALL(4), LABEL("labal_1"), LD(("x",0)), LD(("z",1)), TIMES, RTN, LABEL("labal_2"), LD(("y",2)), LDCI(0), EQ, JOF("labal_3"), LD(("initvalue",4)), GOTO("labal_4"), LABEL("labal_3"), LD(("initvalue",4)), LD(("operation",3)), LD(("y",2)), LDCI(1), MINUS, LD(("x",1)), LD(("recurse",0)), CALL(4), LD(("x",1)), LD(("operation",3)), CALL(2), LABEL("labal_4"), RTN)

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
        case Some (t) => compile(parse(source)) ==List(LDFR(List(),("recurse",0),4,"labal_2"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(0), LDF(Vector(),2,"labal_1"), LDCI(3), LDCI(2), LD(("recurse",0)), TAILCALL(4), LABEL("labal_1"), LD(("x",0)), LD(("z",1)), PLUS, RTN, LABEL("labal_2"), LD(("y",2)), LDCI(0), EQ, JOF("labal_3"), LD(("initvalue",4)), GOTO("labal_4"), LABEL("labal_3"), LD(("initvalue",4)), LD(("operation",3)), LD(("y",2)), LDCI(1), MINUS, LD(("x",1)), LD(("recurse",0)), CALL(4), LD(("x",1)), LD(("operation",3)), CALL(2), LABEL("labal_4"), RTN)

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
        case Some (t) => compile(parse(source)) ==List(LDFR(List(),("recurse",0),4,"labal_2"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(128), LDF(Vector(),2,"labal_1"), LDCI(3), LDCI(2), LD(("recurse",0)), TAILCALL(4), LABEL("labal_1"), LD(("z",1)), LD(("x",0)), DIV, RTN, LABEL("labal_2"), LD(("y",2)), LDCI(0), EQ, JOF("labal_3"), LD(("initvalue",4)), GOTO("labal_4"), LABEL("labal_3"), LD(("initvalue",4)), LD(("operation",3)), LD(("y",2)), LDCI(1), MINUS, LD(("x",1)), LD(("recurse",0)), CALL(4), LD(("x",1)), LD(("operation",3)), CALL(2), LABEL("labal_4"), RTN)

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


}
