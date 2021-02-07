package com.sgw.monads

object OptionExamples extends App {
  // options are like lists with zero or one item

  {
    // an Option[T] has two possible values Some[T] or None
    val optionContainingAString: Option[String] = Some("String")
    val optionContainingNothing: Option[String] = None

    println(optionContainingAString)
    println(optionContainingNothing)
  }

  {
    // instead of ...
    def helloWorld1(name: String): String = {
      if (name == null) {
        return "Hello, World"
      }

      s"Hello, $name"
    }

    // use an option to express that something is optional ...
    def helloWorld2(maybeName: Option[String]): String = {
      s"Hello, ${maybeName.getOrElse("World")}"
    }

    println(helloWorld1(name = null))
    println(helloWorld1(name = "Steve"))

    println(helloWorld2(maybeName = None))
    println(helloWorld2(maybeName = Some("Steve")))
  }

  {
    // options can help you convert from a Java API that might return nulls
    def aJavaAPIThatReturnsNulls(): String = {
      null
    }

    val maybeString = Option(aJavaAPIThatReturnsNulls)

    println(maybeString)
  }

  {
    // options can be converted to lists and then added to other lists
    val o1: Option[String] = Some("Hello")
    val o2: Option[String] = None

    val list = List("A", "B")
    
    val list2 = "A" :: "B" :: Nil
    
    val list3: List[String] = list ++ list2
    
    val list4: List[String] = list ::: list2

    println(o1)
    println(o2)
    println(list)
    println(o1.toList ++ o2.toList ++ list)
  }

  {
    val o1: Option[String] = Some("Steve")
    val o2: Option[String] = None

    // you can use pattern matching on options ...
    o1 match {
      case Some(str) => println(s"Contained a string $str")
      case None => println("Option is empty.")
    }

    o2 match {
      case Some(str) =>
        // do something with the string
        println(s"Contained a string $str")
      case None =>
        // or else do something else
        println("Option is empty.")
    }

    // but ... it's typically better to use map and getOrElse ...
    val whatToSay = o2.map { str =>
      s"Hello, $str"
    }.getOrElse {
      s"Hello, World"
    }

    println(whatToSay)
  }

  {
    // Options are "monads", so they have map and flatMap methods that you can use to compose and map function
    // over options.
    // We've been using map above, but flatMap looks like

    val maybeString1: Option[String] = Some("Hello")
    val maybeString2: Option[String] = Some("Steve")

    // using map we get a Option[Option[String]]
    val withMap: Option[Option[String]] = maybeString1.map { string1 =>
      maybeString2.map { string2 =>
        s"$string1, $string2"
      }
    }

    val flattenedWithMap: Option[String] = withMap.flatten

    println(withMap)

    // using flatMap we get an Option[String]
    val withFlatMap: Option[String] = maybeString1.flatMap { string1 =>
      maybeString2.map { string2 =>
        s"$string1, $string2"
      }
    }

    println(withFlatMap)

    val maybeString3: Option[String] = Some("Hello")
    val maybeString4: Option[String] = None

    // if one or both options are None ...

    // using map we get a Some[None]
    val withMap2: Option[Option[String]] = maybeString3.map { string3 =>
      maybeString4.map { string4 =>
        s"$string3, $string4"
      }
    }

    println(withMap2)

    // using flatMap we get a None
    val withFlatMap2: Option[String] = maybeString3.flatMap { string3 =>
      maybeString4.map { string4 =>
        s"$string3, $string4"
      }
    }

    println(withFlatMap2)
  }

  {
    val maybeString1: Option[String] = Some("Hello")
    val maybeString2: Option[String] = None

    // options can be used in for comprehensions
    val whatToSay = for {
      string1 <- maybeString1
      string2 <- maybeString2
    } yield {
      s"$string1, $string2"
    }

    println(whatToSay)
  }

  {
    // other methods on Option ...
    val maybeString1: Option[String] = Some("Hello")
    val maybeString2: Option[String] = None

    // if a Some is filtered, and it passes the filter, you get a Some ..
    val maybeString3: Option[String] = maybeString1.filter { str =>
      str == "Hello"
    }

    println(maybeString3)

    // if a Some is filered, and it does not pass the filter, you get a None ...
    val maybeString4: Option[String] = maybeString1.filter { str =>
      str == "Foo"
    }

    println(maybeString4)

    // if you start with a None, you always get a None
    val maybeString5: Option[String] = maybeString2.filter { str =>
      str == "Hello"
    }

    println(maybeString5)

    println(maybeString1.contains("Hello"))
    println(maybeString1.contains("Foo"))
  }
}
