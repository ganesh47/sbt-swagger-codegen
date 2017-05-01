# SBT Swagger Code Generator

## Overview

Like the official [swagger-codegen](https://github.com/swagger-api/swagger-codegen) this project aims to generate Scala source code from [Swagger Specification](https://github.com/swagger-api/swagger-spec) compliant API descriptions.
Moreover, you can do it directly within an `sbt` project.

## Compatibility

This code generator is designed specifically for Swagger Spec Version 2.0. Moreover, it relies on [Play! Framework](http://www.playframework.com/) 2.5 for Json marshalling/unmarshalling, server- and client-side code.

## Install

Enable it inside your `project\plugins.sbt` like this:

`resolvers += Resolver.bintrayRepo("th3iedkid","sbt-plugins")
addSbtPlugin("in.thedatateam" % "sbt-swagger-codegen" % "0.0.11")`

Enable it in your `build.sbt` like this:

`enablePlugins(SwaggerCodegenPlugin)`

## Quick start

For a *super fast* hands-on tutorial refer to the related examples and check out [sbt-swagger-codegen-examples](https://github.com/unicredit/sbt-swagger-codegen-examples).

## How it works

By default, the plugin will assume that you have put your `yaml` Swagger specification files under `src/main/swagger`.

If so, then you can just run `swaggerCodeGen` and it will generate your *model's* as case classes under `src/main/scala/swagger/codegen` and [Play Framework](www.playframework.com) [Formats](https://www.playframework.com/documentation/2.4.x/ScalaJsonCombinators#Format) for them (for json serialization) under `src/main/scala/swagger/codegen/json`.

## Tasks

All available tasks from the plugin

- `swaggerModelCodeGen`  -> generated code for model classes
- `swaggerServerCodeGen` -> generates Play Framework code
- `swaggerClientCodeGen` -> generates client code using `play-ws`
- `swaggerRoutesCodeGen` -> generates Play Framework routes
- `swaggerClean` -> cleans up already generated code

The `swaggerModelCodeGen`, `swaggerServerCodeGen` and `swaggerClientCodeGen` will run automatically when the swagger sources change.

`swaggerRoutesCodeGen` will _not_ run automatically because it generates code inside the "src/main/resources" directory instead of in "target/scala-2.12/src_managed".

Tasks can be chained with other tasks (ex: ```(compile in Compile) <<= (compile in Compile) dependsOn swaggerRoutesCodeGen```)

## Keys (and defaults)

These keys influence properties of the generated code itself:

- `swaggerSourcesDir` -> "/src/main/swagger" (path where to search for swagger files)
- `swaggerCodeGenPackage` -> "swagger.codegen" (package name of the generated sources)
- `swaggerModelFileSplitting` -> "singleFile" (in model generation how to group classes in '.scala' files available options are "oneFilePerSource" "oneFilePerModel")
- `swaggerCodeProvidedPackage` -> "com.yourcompany" (where you will provide business logic server method implementation)

These keys determine where generated files will be put:

- `swaggerModelCodeTargetDir` -> "target/scala-2.1x/src_managed/src/main/swagger/model" (path where to put generated model files)
- `swaggerGenerateJsonRW` -> true (if you want to generate json Format for your model case classes)
- `swaggerClientCodeTargetDir` -> "target/scala-2.1x/src_managed/src/main/swagger/client" (path where to put generated client code files)
- `swaggerServerCodeTargetDir` -> "target/scala-2.1x/src_managed/src/main/swagger/server" (path where to put generated server code files)
- `swaggerServerRoutesFile` -> "src/main/resources/routes" (routes file to be generated)

These keys can be used to determine what kind of code should be generated:

- `swaggerGenerateModel` -> true (to be disabled if you do not want model classes to be generated automatically when swagger source code changes)
- `swaggerGenerateClient` -> false (enable this if you want client code to ge generated automatically when swagger source code changes)
- `swaggerGenerateServer` -> false (enable this if you want client code to ge generated automatically when swagger source code changes)

Moreover, you can extend this plugin by providing alternative implementations of the generators via:

- `swaggerModelCodeGenClass` -> new eu.unicredit.swagger.generators.DefaultModelGenerator() (the class used to generate the model classes)
- `swaggerJsonCodeGenClass` -> new eu.unicredit.swagger.generators.DefaultJsonGenerator() (the class used to generate the json marshaller/unmarshaller)
- `swaggerServerCodeGenClass` -> new eu.unicredit.swagger.generators.DefaultServerGenerator() (the class used to generate the Server classes)
- `swaggerClientCodeGenClass` -> new eu.unicredit.swagger.generators.DefaultClientGenerator() (the class used to generate the client classes)

## Dependencies

- scala version 2.11.X
- play-sbt-plugin 2.5.3
- play-json 2.5.3
- play-ws 2.5.3 (only if you use client)

### Limitations

At the moment the project is developed to fullfill some internal projects needs, so do not expect it to cover all the corner cases of the Swagger Spec (i.e. some primitive types in body req or resp).
Model objects must have less than 22 parameters and could fail with circular referencies(due to the Play Json macro limitaion).

## The road ahead

We are actively working with and on this project, trying to overcome any arising limitations and support all Swagger-spec properties we need.
PRs are really welcome and please open an Issue if you find that something is not working.

## Authors:

* Andrea Peruffo: <https://github.com/andreaTP>
* Francesco Montecuccoli Degli Erri <https://github.com/fralken>
* Marco Firrincieli: <https://github.com/mfirry>
* Ganesh: <https://github.com/ganesh47>
*** This is a work in progress and we are not done with it! ***
