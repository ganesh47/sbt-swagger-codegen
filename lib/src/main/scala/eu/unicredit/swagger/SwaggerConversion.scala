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
import io.swagger.models.Model
import treehuggerDSL._
import io.swagger.models.properties._
import io.swagger.models.parameters._

import scala.collection.JavaConversions.mapAsScalaMap
import scala.collection.JavaConverters._

trait SwaggerConversion {

  def propType(p: Property): Type = {
    if (!p.getRequired)
      OptionClass TYPE_OF noOptPropType(p)
    else
      noOptPropType(p)
  }

  private lazy val OffsetDateTimeClass =
    definitions.getClass("java.time.OffsetDateTime")

  private lazy val LocalDateClass =
    definitions.getClass("java.time.LocalDate")

  def noOptPropType(p: Property): Type = {
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
        RootClass.newClass("Map") TYPE_OF(StringClass, noOptPropType(m.getAdditionalProperties))
      case a: ArrayProperty =>
        ListClass TYPE_OF noOptPropType(a.getItems)
      case d: DecimalProperty =>
        BigDecimalClass
      case r: RefProperty =>
        RootClass.newClass(cleanseName(r.getSimpleRef))
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
        val classes = getSeqOfClasses(o.getProperties)
        if (classes.size > 1) RootClass.newClass("Tuple" + classes.size) TYPE_OF (classes: _*) else if (classes.size == 1) classes.head else RootClass.newClass("Any")
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
    if (name.equals("object")) "`object`" else name
  }

  def getSeqOfClasses(items: util.Map[String, Property]): Seq[treehugger.forest.Type] = {
    items.map {
      item => noOptPropType(item._2)
    }.toSeq
  }

  def paramType(p: Parameter): Type = {
    if (!p.getRequired)
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
}
