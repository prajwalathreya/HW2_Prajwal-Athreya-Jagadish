package homework2


import prajwal.homework1_PAJ

import java.io.InvalidClassException
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
    case invokeMethod(i:ourInstanceVariable , methodName:String,parameters:List[Any])

    def evaluate(b :scala.collection.mutable.Map[String,Any] = b): BasicType =
      this match{
        case ourInstanceVariable(instancename) => b(instancename)

        case ourClass(name:String , args*) =>
          if(args.length>0){
            val bMap : scala.collection.mutable.Map[String , Any] = scala.collection.mutable.Map()
            val cMap:scala.collection.mutable.Map[String,Any] = scala.collection.mutable.Map()
            ourmap += key -> cMap
            ourmap += classKey -> name

            args.foreach((args) => args match {
              case ourConstructor(paramList*)=>
                val cMap:scala.collection.mutable.Map[String ,Any] = scala.collection.mutable.Map()
                val dMap:scala.collection.mutable.Map[String,Any]= scala.collection.mutable.Map()
                dMap += "outerScope" -> bMap

            } )

          }



        case ourMethod(ourMethodName,parameters,paramList*)=>



        case ourConstructor(paramList) =>

        case ourField(variablename,value*)=>

        case ourNewObject(ourClass : ourClass)=>

        case invokeMethod(i,methodName,parameters) =>
      }


  //Main method for homework2
  @main def homework2(): Unit =
    import hw2.*







