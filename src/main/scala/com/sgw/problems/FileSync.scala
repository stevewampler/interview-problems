package com.sgw.problems

// Write a function to identify the files that need to be synced from a source system to a destination system (in
// one direction only) based on the files' path and hash.
object FileSync {
  case class File(path: String, hash: Long)

  implicit val fileOrdering = new Ordering[File] {
    override def compare(x: File, y: File): Int = x.path.compareTo(y.path)
  }

  def calcFilesToSync(srcFiles: List[File], dstFiles: List[File]): Set[File] = {
    def go(srcFiles: List[File], dstFiles: List[File]): List[File] = {
      srcFiles.foldLeft(List[File]()) { case (toSync, srcFile) =>
        dstFiles.dropWhile { dstFile =>
          srcFile.path.compareTo(dstFile.path) < 0
        }

        dstFiles.headOption.map { dstFile =>
          if (srcFile.hash != dstFile.hash) {
            srcFile :: toSync
          } else {
            toSync
          }
        }.getOrElse {
          srcFile :: toSync
        }
      }
    }

    go(srcFiles.sorted, dstFiles.sorted).toSet
  }

  def main(args: Array[String]): Unit = {
    val srcFiles = List(
      File("/a", 10),
      File("/b", 20),
      File("/c", 30),
      File("/d", 40)
    )

    val dstFiles = List(
      File("/a", 10),
      File("/c", 40)
    )

    assert(
      calcFilesToSync(srcFiles, dstFiles) == Set(
        File("/b", 20),
        File("/c", 30),
        File("/d", 40)
      )
    )
  }
}
