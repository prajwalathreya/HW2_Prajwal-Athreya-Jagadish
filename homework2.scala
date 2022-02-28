package homework2


import prajwal.homework1_PAJ

import java.io.InvalidClassException
import java.util.NoSuchElementException
import scala.language.implicitConversions
import scala.collection.immutable.ArraySeq


object homework2 :
  type BasicType = Any
  val b : scala.collection.mutable.Map[String,Any] = scala.collection.mutable.Map()

  enum hw2:
    case ourClass(name:String , args :hw2*)
    case ourInstanceVariable(instanceVariableName:String)
    case ourMethod(ourMethodName:String , parameters:List[Any],paramList:Any*)
    case ourConstructor(paramList:Any)
    case ourField(variablename:String , value:Any*)
    case ourNewObject(ourClass: ourClass)
    case invokeMethod(i:ourInstanceVariable , ourmethodName:String,parameters:List[Any])

    def evaluate(b :scala.collection.mutable.Map[String,Any] = b): BasicType =
      this match {
        case ourInstanceVariable(instancename) => b(instancename)

        case ourClass(name: String, args*) =>
          if (args.length > 0) {
            val bMap: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map()
            val c1Map: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map()
            bMap += key -> cMap
            bMap += classKey -> name

            args.foreach((args) => args match {
              case ourConstructor(paramList*) =>
                val cMap: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map()
                val dMap: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map()
                dMap += "outerScope" -> bMap(key).asInstanceOf[scala.collection.mutable.Map[String, BasicType]]

                cMap += key1 -> dMap
                cMap += key2 -> paramList
                bMap += name -> cMap
              case _: Any => println("Error")
            })
            ourClass += name -> bMap
          }


        case ourMethod(ourMethodName, parameters, paramList*) =>
          val eMap: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map()
          val newMethod: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map() ++ parameters.map(paramList *)
          newMethod += "outerScope" -> bMap(newScope).asInstanceOf(scala.collection.mutabe.Map[String, Any])
          eMap += newscope -> newMethod
          eMap += newmethod -> params
          bmap += newmethod -> eMap


        case ourConstructor(paramList) =>

          case ourField(variablename, value*) =>
           if (value, length == 0)
            newVariable (variablename = variablename, value = null).evla (bMap (newscope).asInstanceOf[scala.collection.mmutable.Map[String, Any]] )
           else
            value (0) match {
              case hw2 : Exp => varname(variableName = variablenName,value = hw2).evaluate)classMap().asInstanceOf[scala.colection.mutable.Map]
              case_: Any => print("ellaru prong in")
        case _: Amy => println("Error")

      }

        case ourNewObject(ourClass : ourClass)=>
          ourClass.evaluate()match{
            case bMap : scala.collection.mutable.Map[String,Any] =>

              val ourClassName = bMap(Name).asInstanceOf[String]
              val ourConstructor = bMap(ourClassName).asInstanceOf[scala.collection.mutable.Map[String,Any]]
              val ourConstructorLine:ArraySeq[Any] = ourConstructor().asInstanceOf[ArraySeq[Any]]

            case _: Any => throw NoSuchElementException()
          }

        case invokeMethod(i,ourmethodName,parameters) =>
          ourInstanceVariable.evaluate(ourmethodName) match{
            case b:scala.collection.mutable.Map[String,Any] =>
              b.get(ourmethodname)

            case_: println("Error")
          }

      }


  //Main method for homework2
  @main def homework2(): Unit =
    import hw2.*







