package com.sgw.problems

// Write a function "calcFilesToSync" that identifies the files that need to be synced from a source system to a destination system (in
// one direction only) based on the files' path and hash.
object FileSync {
  case class File(path: String, hash: Long)

  implicit val fileOrdering = new Ordering[File] {
    override def compare(x: File, y: File): Int = x.path.compareTo(y.path)
  }

  def calcFilesToSync(srcFiles: List[File], dstFiles: List[File]): Set[File] = {
//    def go(srcFiles: List[File], dstFiles: List[File]): List[File] = {
//      srcFiles.foldLeft(List[File](), dstFiles) { case ((toSync, dstFiles), srcFile) =>
//        val newDstFiles = dstFiles.dropWhile { dstFile =>
//          srcFile.path.compareTo(dstFile.path) < 0
//        }
//
//        newDstFiles.headOption.map { dstFile =>
//          if (srcFile.hash != dstFile.hash) {
//            (srcFile :: toSync, newDstFiles.tail)
//          } else {
//            (toSync, newDstFiles.tail)
//          }
//        }.getOrElse {
//          (srcFile :: toSync, newDstFiles)
//        }
//      }._1
//    }
//
//    go(srcFiles.sorted, dstFiles.sorted).toSet
    srcFiles.toSet diff dstFiles.toSet
  }

  def main(args: Array[String]): Unit = {
    // none of the files have changed or are new, so no files need to be sync'd
    assert(
      calcFilesToSync(
        srcFiles = List(
          File("/a", 10),
          File("/b", 20),
          File("/c", 30),
          File("/d", 40)
        ),
        dstFiles = List(
          File("/a", 10),
          File("/b", 20),
          File("/c", 30),
          File("/d", 40)
        )
      ) == Set(
      )
    )

    // all of the files are new and should be sync'd
    assert(
      calcFilesToSync(
        srcFiles = List(
          File("/a", 10),
          File("/b", 20),
          File("/c", 30),
          File("/d", 40)
        ),
        dstFiles = List(
        )
      ) == Set(
        File("/a", 10),
        File("/b", 20),
        File("/c", 30),
        File("/d", 40)
      )
    )

    // all of the files have changed, so all of them need to be sync'd
    assert(
      calcFilesToSync(
        srcFiles = List(
          File("/a", 10),
          File("/b", 20),
          File("/c", 30),
          File("/d", 40)
        ),
        dstFiles = List(
          File("/a", 100),
          File("/b", 200),
          File("/c", 300),
          File("/d", 400)
        )
      ) == Set(
        File("/a", 10),
        File("/b", 20),
        File("/c", 30),
        File("/d", 40)
      )
    )

    // two of the files are new ("/b" and "/d") and one ("/c") has changed, so those need to be sync'd
    assert(
      calcFilesToSync(
        srcFiles = List(
          File("/a", 10),
          File("/b", 20),
          File("/c", 30),
          File("/d", 40)
        ),
        dstFiles = List(
          File("/a", 10),
          File("/c", 40)
        )
      ) == Set(
        File("/b", 20),
        File("/c", 30),
        File("/d", 40)
      )
    )
  }
}
