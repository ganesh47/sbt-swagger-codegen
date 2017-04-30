import eu.unicredit.swagger.generators.DefaultModelGenerator

/**
  * Created by Ganesh on 4/30/2017.
  */
object SampleYamlTest {
  def main(args: Array[String]): Unit = {

    val generator = new DefaultModelGenerator()
    generator.generate("lib/src/test/resources/bitbucket.yaml","target").foreach{
      x=> println(x.code)
    }
  }
}
