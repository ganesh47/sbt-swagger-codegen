import eu.unicredit.swagger.generators.{DefaultJsonGenerator, DefaultModelGenerator}

/**
  * Created by Ganesh on 4/30/2017.
  */
object SampleYamlTest {
  def main(args: Array[String]): Unit = {

    val generatorM = new DefaultModelGenerator()
    generatorM.generate("lib/src/test/resources/bitbucket.yaml", "target").foreach {
      x => println(x.code)
    }

    val generatorJM = new DefaultJsonGenerator()
    generatorJM.generate("lib/src/test/resources/bitbucket.yaml", "target").foreach {
      x => println(x.code)
    }

  }
}
