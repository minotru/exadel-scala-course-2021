package scala2021.skarasik.task07

import scala.language.implicitConversions
import scala.util.Try

class SomeResource(val throwException: Boolean = false) {
  def run(): Unit = {
    if (throwException) {
      throw new Exception("Whoooh, some exception!")
    } else {
      println(s"SomeResource.run is called")
    }
  }

  def close(): Unit = {
    println(s"SomeResource.close is called")
  }
}

object ManagedResource {
  // todo: how to properly define this implicit???
  implicit def defaultClose[R](resource: R => Unit): Unit = {
    println("Default close is used")
  }

  def main(args: Array[String]): Unit = {
    println("withResource without exception")
    withResource(new SomeResource())(r => r.run())(r => r.close())
    println()

    println("withResource with exception in run")
    withResource(new SomeResource(throwException = true))(r => r.run())(r => r.close())
    println()

    println("withResource with exception in init")
    withResource({
      throw new Exception("in init")
      new SomeResource()
    })(r => r.run())(r => r.close())
    println()

    println("withResource with exception in close")
    withResource(new SomeResource())(r => r.run())(r => {
      r.close()
      throw new Exception("in close")
    })
  }

  def withResource[R, O](createResource: => R)(useResource: R => O)(implicit closeResource: R => Unit): Try[O] = {
    Try {
      val resource = createResource
      try {
        useResource(resource)
      } finally {
        closeResource(resource)
      }
    }
  }
}
