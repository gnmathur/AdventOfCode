package aoc2022

import utils.AocFileOps

import scala.collection.mutable
import scala.util.{Failure, Success}

/**
 * A mutable-state solution to Day 07
 */
object D07NoSpaceLeftOnDevice extends App {
  val CmdCdRegex = """\$ cd ([a-z/(..)]+)""".r
  val CmdLsRegex = """\$ ls""".r
  val ContentDirRegex = """dir ([-_.A-Za-z0-9]+)""".r
  val ContentFileNameRegex = """([0-9]+) ([-_.A-Za-z0-9]+)""".r

  case class File(name: String, size: Long)
  case class Directory(name: String, files: mutable.Set[File], subDirectories: mutable.Set[Directory]) {
    def size: Long = files.map(_.size).sum + subDirectories.map(_.size).sum
  }

  val fileSystem = mutable.Map[String, Directory]()

  def parse(input: List[String]): Directory = {
    val traverseStack = mutable.Stack[Directory]()
    val rootDir = Directory("/", mutable.Set(), mutable.Set())

    traverseStack.push(rootDir)

    input.drop(1).foreach {
      case CmdCdRegex(dirName) => dirName match {
        case ".." => traverseStack.pop()
        case "/" => traverseStack.popWhile(_.name != "/")
        case _ =>
          val path = if (traverseStack.size > 1) traverseStack.top.name + "/" + dirName else "/" + dirName
          val dir = fileSystem.getOrElse(path, Directory(path, mutable.Set(), mutable.Set()))
          if (!fileSystem.contains(path)) {
            traverseStack.top.subDirectories.add(dir)
          }
          traverseStack.push(dir)
      }
      case ContentFileNameRegex(size, fileName) =>
        traverseStack.top.files.add(File(fileName, size.toLong))
      case ContentDirRegex(dirName) => // noop
      case CmdLsRegex() => // noop
    }
    rootDir
  }

  def solvePart1(input: List[String]): Long = {
    def visit(d: Directory): Long = {
      val subDirSum = d.subDirectories.map(visit).sum
      if (d.size <= 100000) subDirSum + d.size else subDirSum
    }

    val root = parse(input)
    visit(root)
  }

  def solvePart2(input: List[String]): Long = {
    def visit(d: Directory): List[Long] = {
      val s = d.subDirectories.flatMap(visit)
      List(d.size) ++ s.toList
    }

    val root = parse(input)
    val currentFree = 70000000L - root.size
    visit(root).filter(ele => (ele+currentFree) >= 30000000L).min
  }

  // Test cases
  AocFileOps
    .readInputAsStringList("src/main/resources/aoc2022/2022D07TestInput.lst") match {
    case Success(input) =>
      assert(95437 == solvePart1(input))
      assert(24933642 == solvePart2(input))
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }

  // Solution to part 1 and 2
  AocFileOps
    .readInputAsStringList("src/main/resources/aoc2022/2022D07Input.lst") match {
    case Success(input) =>
      assert(1915606 == solvePart1(input))
      assert(5025657 == solvePart2(input))
    case Failure(exception) => println(s"Error parsing test input (error: ${exception})")
  }

}
