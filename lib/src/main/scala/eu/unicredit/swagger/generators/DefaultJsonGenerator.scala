/* Copyright 2015 UniCredit S.p.A.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package eu.unicredit.swagger.generators


import eu.unicredit.swagger.SwaggerConversion
import treehugger.forest._
import definitions._
import io.swagger.models.{Model, ModelImpl}
import io.swagger.models.properties.{ObjectProperty, Property, RefProperty}
import io.swagger.parser.SwaggerParser
import treehuggerDSL._

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

class DefaultJsonGenerator extends JsonGenerator with SwaggerConversion {

  def generateJsonInit(packageName: String): String = {
    val initTree =
      BLOCK {
        Seq(IMPORT("play.api.libs.json", "_"))
      } inPackage packageName

    treeToString(initTree)
  }

  def generateJsonImplicits(vds: List[ValDef]): String = {
    val tree = PACKAGEOBJECTDEF("json") := BLOCK(vds)

    treeToString(tree)
  }


  def generateJsonRW(fileName: String): List[ValOrDefDef] = {
    val swagger = new SwaggerParser().read(fileName)

    val models = swagger.getDefinitions.asScala

    val realModels = new collection.mutable.HashMap[String, Model]

    //prepare tuple reads and writes for context

    val counters: scala.collection.mutable.Set[Int] = new scala.collection.mutable.HashSet()
    val maxPropCount = models.map(x => parseComposedModels(models, x._2))
      .map(x => x.getProperties.asScala)
      .filter(_.exists(_._2.isInstanceOf[ObjectProperty]))
      .map(_.size)
      .max
    (2 to maxPropCount).map(counters.add)


    val tupleDefs: List[ValOrDefDef] = counters.dropWhile(_ == 0).foldRight(List.empty[ValOrDefDef]) {
      (x, y) => prepareJsonTree(1, x) ++ y
    }

    models.filter(x => x._2 == null).foreach(x => realModels.put(x._1, x._2))
    models.filter(x => x._2.getProperties != null).filter(x => x._2.getProperties.asScala.exists(xx => xx._2.isInstanceOf[RefProperty])).foreach {
      modelWithRef =>
        val properties = modelWithRef._2.getProperties.asScala
        properties.filter(_._2.isInstanceOf[RefProperty]).map(_._2.asInstanceOf[RefProperty]).map {
          x =>
            if (models.contains(x.getSimpleRef)) {
              realModels.put(x.getSimpleRef, models(x.getSimpleRef))
            }
        }
        realModels.put(modelWithRef._1, modelWithRef._2)
    }
    models.filter(x => !realModels.contains(x._1)).foreach {
      each =>
        realModels.put(each._1, each._2)
    }
    val swaggerModels = (for {
      (name, model) <- realModels.map(x => (x._1, parseComposedModels(models, x._2)))
      c <- Seq("Reads", "Writes")
    } yield {
      var properties = getProperties(model)
      parseForProperty(name, c, properties)
    }).toList

    tupleDefs ++ swaggerModels
  }

  private def parseForProperty(name: String, c: String, properties: Iterable[(String, Property)]): ValOrDefDef = {
    val caseObject = false
    val typeName = if (caseObject) s"$name.type" else if (name.equals("object")) "`object`" else name
    extractVal(name, c, properties, caseObject, typeName)
  }

  private def extractVal(name: String, c: String, properties: Iterable[(String, Property)], caseObject: Boolean, typeName: String): ValOrDefDef = {
    VAL(s"$name$c", s"$c[$typeName]") withFlags(Flags.IMPLICIT, Flags.LAZY) := {
      c match {
        case "Reads" =>
          val param = VAL("json").tree
          ANONDEF(s"$c[$typeName]") :=
            LAMBDA(param) ==>
              REF("JsSuccess") APPLY {
              if (caseObject) {
                REF(name)
              } else {
                REF(name) APPLY {
                  for ((pname, prop) <- properties) yield {
                    val mtd = if (!prop.getRequired && !prop.isInstanceOf[ObjectProperty]) "asOpt" else "as"
                    PAREN(REF("json") INFIX("\\", LIT(pname))) DOT mtd APPLYTYPE noOptPropType(prop)
                  }
                }
              }
            }
        case "Writes" =>
          val param = VAL("o").tree
          ANONDEF(s"$c[$typeName]") :=
            LAMBDA(param) ==>
              REF("JsObject") APPLY {
              if (caseObject) {
                SeqClass APPLY Seq.empty
              } else {
                SeqClass APPLY {
                  for ((pname, prop) <- properties)
                    yield {
                      LIT(pname) INFIX("->", (REF("Json") DOT "toJson") (REF("o") DOT pname))
                    }
                } DOT "filter" APPLY (REF("_") DOT "_2" INFIX("!=", REF("JsNull")))
              }
            }
      }
    }
  }

  def generateJson(destPackage: String, vds: List[ValOrDefDef]): Iterable[SyntaxString] = {
    val pre = generateJsonInit(destPackage)

    val tree = PACKAGEOBJECTDEF("json") := BLOCK(vds)

    val code = treeToString(tree)
    Seq(SyntaxString("json", pre, code))
  }

  def generate(fileName: String, destPackage: String): Iterable[SyntaxString] = {
    generateJson(destPackage, generateJsonRW(fileName))
  }

  /**
    * Prepare a Json tuple reads tree
    *
    * @param offset
    * @param limit
    * @return
    */
  def prepareJsonTree(offset: Int, limit: Int): List[ValOrDefDef] = {
    val params = (for (i <- offset to limit) yield VAL(s"a${
      i
    }Reads", s"Reads[A$i]") withFlags Flags.IMPLICIT) map (_.tree)
    val yieldValues = (for (i <- (offset + 1) to limit) yield s"a$i").fold("a1")((x, y) => x + "," + y)
    val caseMatherOne = FOR(
      for (i <- offset to limit) yield VALFROM(s"a$i") := REF(s"a${
        i
      }Reads.reads(arr(${
        i - offset
      }))")
    ) YIELD REF("(" + yieldValues + ")")
    val positiveCase = CASE(RootClass.newClass("JsArray") UNAPPLY ID("arr"), IF(REF("arr.size") ANY_== LIT(limit))) ==> caseMatherOne
    val negativeCase = CASE(REF("_")) ==> REF("JsError(Seq(JsPath() -> Seq(JsonValidationError(\"Expected array of " + limit + " elements\"))))")

    val rhs = ANONDEF(s"Reads[Tuple$limit[" + yieldValues.toUpperCase + "]]") := BLOCK(positiveCase, negativeCase)
    val start = DEF(s"tuple${
      limit
    }Reads[${
      yieldValues.toUpperCase
    }]", s"Reads[Tuple$limit[${
      yieldValues.toUpperCase
    }]]") withFlags Flags.IMPLICIT withParams (params: _*) := rhs
    val vals = (for (i <- offset to limit) yield VAL(s"a${i}Writes", s"Writes[A$i]") withFlags Flags.IMPLICIT) map (_.tree)
    val typeString = s"Tuple$limit[${yieldValues.toUpperCase}]"
    val writesVals = Seq(VAL("tuple", typeString)) map (_.tree)
    val returnString = (for (i <- (offset + 1) to limit) yield s"a${i}Writes.writes(tuple._$i)").fold("a1Writes.writes(tuple._1)")((x, y) => x + "," + y)
    val writesMethod = DEF("writes") withParams writesVals := REF(" JsArray(Seq(" + returnString + "))")
    val writesRhs = NEW(ANONDEF(s"Writes[Tuple$limit[${yieldValues.toUpperCase}]]") := BLOCK(writesMethod))
    val writes = DEF(s"tuples${limit}Writes[${yieldValues.toUpperCase}]") withFlags Flags.IMPLICIT withParams vals := writesRhs
    List(start, writes)
  }

}
