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

import java.util

import eu.unicredit.swagger.SwaggerConversion
import treehugger.forest._
import definitions._
import io.swagger.models.{ComposedModel, Model, RefModel}
import treehuggerDSL._
import io.swagger.parser.SwaggerParser
import io.swagger.models.properties._

import scala.collection.JavaConverters._
import scala.collection.mutable


class SuperComposedModel extends ComposedModel {
  private var properties: java.util.Map[String, Property] = null

  def populate(composed: ComposedModel): Unit = {
    super.setAllOf(composed.getAllOf)
    super.setParent(composed.getParent)
    super.setDescription(composed.getDescription)
    super.setExample(composed.getExample)
    super.setInterfaces(composed.getInterfaces)
    super.setChild(composed.getChild)
    super.setExternalDocs(composed.getExternalDocs)
    super.setReference(composed.getReference)
    super.setTitle(composed.getTitle)
    super.setVendorExtensions(composed.getVendorExtensions)
  }

  override def setProperties(properties: util.Map[String, Property]): Unit = {
    this.properties = properties
  }

  override def getProperties: util.Map[String, Property] = properties
}

class DefaultModelGenerator extends ModelGenerator with SwaggerConversion {

  def generateClass(name: String, props: Iterable[(String, Property)], comments: Option[String]): String = {
    val GenClass = RootClass.newClass(name)

    val params: Iterable[ValDef] = for ((pname, prop) <- props)
      yield PARAM(pname, propType(prop)): ValDef

    val tree: Tree =
      if (params.isEmpty)
        OBJECTDEF(GenClass) withFlags Flags.CASE
      else
        CLASSDEF(GenClass) withFlags Flags.CASE withParams params

    val resTree = comments.map(tree withComment _).getOrElse(tree)

    treeToString(resTree)
  }

  def generateModelInit(packageName: String): String = {
    //val initTree =
    //PACKAGE(packageName)

    //treeToString(initTree)
    "package " + packageName
  }

  def generate(fileName: String, destPackage: String): Iterable[SyntaxString] = {
    val swagger = new SwaggerParser().read(fileName)

    val models = swagger.getDefinitions.asScala

    val finalOnes = models.map {
      x =>
        (x._1, parseComposedModels(models, x._2))
    }
    for {
      (name, model) <- finalOnes
    } yield
      SyntaxString(name + ".scala",
        generateModelInit(destPackage),
        generateClass(name, getProperties(model), Option(model.getDescription)))
  }


}
