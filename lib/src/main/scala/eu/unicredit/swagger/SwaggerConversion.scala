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
package eu.unicredit.swagger

import java.util

import treehugger.forest._
import definitions._
import eu.unicredit.swagger.generators.SuperComposedModel
import io.swagger.models.{ComposedModel, Model, RefModel}
import treehuggerDSL._
import io.swagger.models.properties._
import io.swagger.models.parameters._

import scala.collection.JavaConversions.mapAsScalaMap
import scala.collection.JavaConverters._
import scala.collection.mutable

trait SwaggerConversion {

  def propType(p: Property): Type = {
    if (!p.getRequired )
      OptionClass TYPE_OF noOptPropType(p)
    else
      noOptPropType(p)
  }

  private lazy val OffsetDateTimeClass =
    definitions.getClass("java.time.OffsetDateTime")

  private lazy val LocalDateClass =
    definitions.getClass("java.time.LocalDate")

  def noOptPropType(p: Property,system:String=""): Type = {
    import collection.JavaConverters._
    p match {
      case s: StringProperty =>
        StringClass
      case b: BooleanProperty =>
        BooleanClass
      case d: DoubleProperty =>
        DoubleClass
      case f: FloatProperty =>
        FloatClass
      case i: IntegerProperty =>
        IntClass
      case l: LongProperty =>
        LongClass
      case i: BaseIntegerProperty =>
        IntClass
      case m: MapProperty =>
        RootClass.newClass("Map") TYPE_OF(StringClass, (if(system.length>0) system+"." else "")+noOptPropType(m.getAdditionalProperties))
      case a: ArrayProperty =>
        ListClass TYPE_OF noOptPropType(a.getItems,system)
      case d: DecimalProperty =>
        BigDecimalClass
      case r: RefProperty =>
        RootClass.newClass((if(system.length>0) system+"." else "") + cleanseName(r.getSimpleRef))
      case dt: DateProperty =>
        LocalDateClass
      case dt: DateTimeProperty =>
        OffsetDateTimeClass

      case ba: ByteArrayProperty =>
        throw new Exception(s"ByteArrayProperty $p is not supported yet")
      case b: BinaryProperty =>
        throw new Exception(s"BinaryProperty $p is not supported yet")
      // supported as a subclass of StringProperty
      //case e: EmailProperty =>
      //  throw new Exception(s"EmailProperty $p is not supported yet")
      case f: FileProperty =>
        throw new Exception(s"FileProperty $p is not supported yet")
      case o: ObjectProperty =>
        val classes = getSeqOfClasses(o.getProperties,system)
        if (classes.size > 1) RootClass.newClass("Tuple" + classes.size) TYPE_OF (classes: _*) else if (classes.size == 1) classes.head else RootClass.newClass("String")
      case p: PasswordProperty =>
        throw new Exception(s"PasswordProperty $p is not supported yet")
      case u: UUIDProperty =>
        throw new Exception(s"UUIDProperty $p is not supported yet")

      case null =>
        throw new Exception("Trying to resolve null property")
      case x =>
        // should not happen as all existing types have been checked before
        throw new Exception(s"unexpected property type $x")
    }
  }

  def cleanseName(name: String): String = {
    if (name.equals("object")) "`object`" else if (name.contains("-")||name.equals("type")) "`"+name+"`" else name
  }
  def cleanseProp(name: String): String = {
    if (name.equals("object")) "`object`" else if (name.contains("-")||name.equals("type")) "`"+name+"`" else name
  }

  def getSeqOfClasses(items: util.Map[String, Property],system:String=""): Seq[treehugger.forest.Type] = {
    items.map {
      item => noOptPropType(item._2,system)
    }.toSeq
  }

  def paramType(p: Parameter): Type = {
    if (!p.getRequired )
      OptionClass TYPE_OF noOptParamType(p)
    else
      noOptParamType(p)
  }

  def noOptParamType(p: Parameter): Type = {
    p match {
      case asp: AbstractSerializableParameter[_] =>
        if (asp.getType == "array")
          ListClass TYPE_OF noOptPropType(asp.getItems)
        else
          noOptPropType(PropertyBuilder.build(asp.getType, asp.getFormat, null))
      case bp: BodyParameter =>
        noOptPropType(new RefProperty(bp.getSchema.getReference))
      case rp: RefParameter =>
        RootClass.newClass(rp.getSimpleRef)
    }
  }

  def getProperties(model: Model): Iterable[(String, Property)] = {
    val props = model.getProperties
    if (props == null) Iterable.empty else props.asScala
  }

  def parseComposedModels(models: mutable.Map[String, Model], model: Model): Model = {
    model match {
      case model: ComposedModel =>
        val props = if (null == model.getProperties) new util.HashMap[String, Property]() else model.getProperties
        model.asInstanceOf[ComposedModel].getAllOf.asScala.foreach {
          each =>
            val items = parseRefModel(models, if (each.isInstanceOf[ComposedModel]) parseComposedModels(models, parseRefModel(models, each)) else each)
            if (items!=null && items.getProperties != null && items != model )
              props.putAll(items.getProperties)
        }
        val model1 = new SuperComposedModel
        model1.populate(model.asInstanceOf[ComposedModel])
        model1.setProperties(props)
        model1
      case _ => model
    }
  }

  def parseRefModel(models: mutable.Map[String, Model], model: Model): Model = {
    model match {
      case model1: RefModel => parseRefModel(models, models(model1.getSimpleRef))
      case _ => if (model.isInstanceOf[ComposedModel]) parseComposedModels(models, model) else model
    }
  }
}
