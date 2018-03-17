import java.nio.file.{Path, Paths, Files}
import org.scalatest.{FlatSpec, DiagrammedAssertions, Matchers}

import jp.gr.java_conf.polyhedron.splittersprite3.Atmosphere
import jp.gr.java_conf.polyhedron.splittersprite3.agent
import jp.gr.java_conf.polyhedron.splittersprite3.outerspace

class IOUtilsSpec extends FlatSpec with DiagrammedAssertions with Matchers {
  // バージョンの増加順のリスト
  val validVerNameAndAnswerList = List(
    ("ver0.0.0", (0, 0, 0)),
    ("ver1.0.0", (1, 0, 0)),
    ("ver1.0.1", (1, 0, 1)),
    ("ver1.0.10", (1, 0, 10)),
    ("ver1.1.0", (1, 1, 0)),
    ("ver1.10.0", (1, 10, 0)),
    ("ver2.0.0", (2, 0, 0)),
    ("ver10.0.0", (10, 0, 0)),
    ("ver10.11.12", (10, 11, 12)),
  )

  val invalidVerNameList = List(
    // 全角文字禁止
    "ver１.０.０", "ｖｅｒ１.０.０", "ｖｅｒ１．０．０",
    // verで始まること
    "ve1.0.0", "1.0.0", "Ver1.0.0", "VER1.0.0",
    // verのあとにピリオド区切りで３つの文字列が続くこと
    "ver1.0", "ver1.0.0.0",
    // ３つの文字列は数値として解釈できること
    "ver.0.0", "ver1..0", "ver1.0.",
    "verX.0.0", "ver1.Y.0", "ver1.0.Z",
    // ３つの文字列は非負の整数であること
    "ver-1.0.0", "ver1.-2.0", "ver1.0.-3",
    // 一意的にならない整数表現は禁止
    "ver00.0.0", "ver01.0.0", "ver0.00.0",
    "ver0.01.0", "ver0.0.00", "ver0.0.01",
  )

  val pathTailMap = Map(
    (Paths.get("foo") -> "foo"),
    (Paths.get("foo", "bar") -> "bar"),
    (Paths.get("foo", "bar", "buz") -> "buz"),
    (Paths.get("foo", "bar", "ほげ") -> "ほげ"),
    (Paths.get("foo", "ほげ", "buz") -> "buz"),
    (Paths.get("foo", "ほげ", "ふが") -> "ふが"),
  )

  val validPatchNameAndAnswerList =
    (for (((fromName, fromVersion), fromIndex) <-
         validVerNameAndAnswerList.zipWithIndex;
         ((toName, toVersion), toIndex) <-
         validVerNameAndAnswerList.zipWithIndex
         if fromIndex < toIndex) yield {
      (s"patch_from_${fromName}_to_${toName}", (fromVersion, toVersion))
    }).toList

  val invalidPatchNameList =
    // 全角文字
    "ｐａｔｃｈ＿ｆｒｏｍ＿ver1.0.0＿ｔｏ＿ver2.0.0" ::
    // 大文字始まり
    "Patch_from_ver1.0.0_to_ver2.0.0" ::
    // 区切りの個数不足
    "patch_from_ver1.0.0" ::
    // toのつづり間違い
    "patch_from_ver1.0.0_too_ver2.0.0" ::
    // バージョン名が片方不正
    (for (validVerName <- validVerNameAndAnswerList.map(_._1);
         invalidVerName <- invalidVerNameList) yield {
      s"patch_from_${validVerName}_to_${invalidVerName}"
      s"patch_from_${invalidVerName}_to_${validVerName}"
    }).toList :::
    // バージョン名が両方不正
    (for (invalidVerNameX <- invalidVerNameList;
         invalidVerNameY <- invalidVerNameList) yield {
      s"patch_from_${invalidVerNameX}_to_${invalidVerNameY}"
    }).toList :::
    // バージョンの値が増加していない
    (for (((fromName, fromVersion), fromIndex) <-
         validVerNameAndAnswerList.zipWithIndex;
         ((toName, toVersion), toIndex) <-
         validVerNameAndAnswerList.zipWithIndex
         if fromIndex >= toIndex) yield {
      s"patch_from_${fromName}_to_${toName}"
    }).toList

  "IOUtils" should "tailNameOfでPathの末尾の名前を取得" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        for ((path, name) <- pathTailMap) {
          assert(Atmosphere.ioUtils.tailNameOf(path) === name)
          val absPath = path.toAbsolutePath()
          assert(Atmosphere.ioUtils.tailNameOf(absPath) === name)
        }
      }
    }
  }

  "IOUtils" should "文字列\"verA.B.C\"は(A, B, C)にパースされる" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        for ((name, answer) <- validVerNameAndAnswerList) {
          assert(Atmosphere.ioUtils.parseVersionName(name) === answer)
        }
      }
    }
  }

  "IOUtils" should "不正なバージョン名の文字列はInvalidVersionName例外" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        for (name <- invalidVerNameList) {
          val e = intercept[outerspace.IOUtils.InvalidVersionName] {
            Atmosphere.ioUtils.parseVersionName(name)
          }
          assert(e.name === name)
        }
      }
    }
  }

  "IOUtils" should "ディレクトリ名\"verA.B.C\"は(A, B, C)にパースされる" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        for ((name, answer) <- validVerNameAndAnswerList;
            parentDir <- pathTailMap.keys) {
          val dir = parentDir.resolve(name)
          assert(Atmosphere.ioUtils.parseVersionDirectory(dir) === answer)
          val absDir = dir.toAbsolutePath
          assert(Atmosphere.ioUtils.parseVersionDirectory(absDir) === answer)
        }
      }
    }
  }

  "IOUtils" should
      "不正なバージョン名のディレクトリはInvalidVersionDirectory例外" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        for (name <- invalidVerNameList; parentDir <- pathTailMap.keys) {
          val dir = parentDir.resolve(name)
          val e = intercept[outerspace.IOUtils.InvalidVersionDirectory] {
            Atmosphere.ioUtils.parseVersionDirectory(dir)
          }
          assert(e.path === dir)

          val absDir = dir.toAbsolutePath
          val absE = intercept[outerspace.IOUtils.InvalidVersionDirectory] {
            Atmosphere.ioUtils.parseVersionDirectory(absDir)
          }
          assert(absE.path === absDir)
        }
      }
    }
  }

  "IOUtils" should
      ("ディレクトリ名\"patch_from_verA.B.C_to_verX.Y.Z\"は" +
      "((A, B, C), (X, Y, Z))にパースされる") in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        for ((patchName, answer) <- validPatchNameAndAnswerList;
            parentDir <- pathTailMap.keys) {
          val dir = parentDir.resolve(patchName)
          assert(Atmosphere.ioUtils.parsePatchDirectory(dir) === answer)
          assert(Atmosphere.ioUtils.parsePatchDirectory(dir.toAbsolutePath) ===
                 answer)
        }
      }
    }
  }

  "IOUtils" should
      "不正なパッチ名のディレクトリはInvalidPatchDirectory例外" in {
    Atmosphere.withTestIOUtils {
      agent.LoanAgent.loan {
        for (patchName <- invalidVerNameList; parentDir <- pathTailMap.keys) {
          val dir = parentDir.resolve(patchName)

          val e = intercept[outerspace.IOUtils.InvalidPatchDirectory] {
            Atmosphere.ioUtils.parsePatchDirectory(dir)
          }
          assert(e.path === dir)

          val absE = intercept[outerspace.IOUtils.InvalidPatchDirectory] {
            Atmosphere.ioUtils.parsePatchDirectory(dir.toAbsolutePath)
          }
          assert(absE.path === dir.toAbsolutePath)
        }
      }
    }
  }

  "IOUtils" should "バージョンディレクトリを判別可能" in {
    for (parentDir <- pathTailMap.keys) {
      Atmosphere.withTestIOUtils(
          parentDir, Paths.get("ver1.0.0", "game.jar")) {
        agent.LoanAgent.loan {
          for (name <- validVerNameAndAnswerList.map(_._1)) {
            val path = Atmosphere.ioUtils.gameDirPath.resolve(name)

            // 何も存在しないなら偽判定
            assert(!Atmosphere.ioUtils.isVersionDirectory(path))

            // ファイルが存在なら偽判定
            Files.createFile(path)
            assert(!Atmosphere.ioUtils.isVersionDirectory(path))
            Files.delete(path)

            // ディレクトリが存在なら真判定
            Files.createDirectories(path)
            assert(Atmosphere.ioUtils.isVersionDirectory(path))
            Files.delete(path)
          }

          for (name <-
              invalidVerNameList ++ validPatchNameAndAnswerList.map(_._1)) {
            val path = Atmosphere.ioUtils.gameDirPath.resolve(name)

            // 何も存在しないなら偽判定
            assert(!Atmosphere.ioUtils.isVersionDirectory(path))

            // ファイルが存在なら偽判定
            Files.createFile(path)
            assert(!Atmosphere.ioUtils.isVersionDirectory(path))
            Files.delete(path)

            // ディレクトリが存在なら偽判定
            Files.createDirectories(path)
            assert(!Atmosphere.ioUtils.isVersionDirectory(path))
            Files.delete(path)
          }
        }
      }
    }
  }

  "IOUtils" should "パッチディレクトリを判別可能" in {
    for (parentDir <- pathTailMap.keys) {
      Atmosphere.withTestIOUtils(
          parentDir, Paths.get("ver1.0.0", "game.jar")) {
        agent.LoanAgent.loan {
          for (name <- validPatchNameAndAnswerList.map(_._1)) {
            val path = Atmosphere.ioUtils.gameDirPath.resolve(name)

            // 何も存在しないなら偽判定
            assert(!Atmosphere.ioUtils.isPatchDirectory(path))

            // ファイルが存在なら偽判定
            Files.createFile(path)
            assert(!Atmosphere.ioUtils.isPatchDirectory(path))
            Files.delete(path)

            // ディレクトリが存在なら真判定
            Files.createDirectories(path)
            assert(Atmosphere.ioUtils.isPatchDirectory(path))
            Files.delete(path)
          }

          for (name <-
              invalidPatchNameList ++ validVerNameAndAnswerList.map(_._1)) {
            val path = Atmosphere.ioUtils.gameDirPath.resolve(name)

            // 何も存在しないなら偽判定
            assert(!Atmosphere.ioUtils.isPatchDirectory(path))

            // ファイルが存在なら偽判定
            Files.createFile(path)
            assert(!Atmosphere.ioUtils.isPatchDirectory(path))
            Files.delete(path)

            // ディレクトリが存在なら偽判定
            Files.createDirectories(path)
            assert(!Atmosphere.ioUtils.isPatchDirectory(path))
            Files.delete(path)
          }
        }
      }
    }
  }

  "IOUtils" should "パッチチェーンを取得可能" in {
    def assertPatchChain(
        verOrPatchDirName: String, otherDirNames: Set[String],
        otherFileNames: Set[String], expectedChainNames: List[String]) {
      for (parentDir <- pathTailMap.keys) {
        Atmosphere.withTestIOUtils(
            parentDir, Paths.get(verOrPatchDirName, "game.jar")) {
          agent.LoanAgent.loan {
            Files.createDirectories(
              Atmosphere.ioUtils.gameDirPath.resolve(verOrPatchDirName))
            for (dirName <- otherDirNames) {
              val path = Atmosphere.ioUtils.gameDirPath.resolve(dirName)
              Files.createDirectories(path)
            }
            for (fileName <- otherFileNames) {
              val path = Atmosphere.ioUtils.gameDirPath.resolve(fileName)
              Files.createFile(path)
            }

            Atmosphere.ioUtils.appliedPatchDirList should be (
              expectedChainNames.map(Atmosphere.ioUtils.gameDirPath.resolve))
          }
        }
      }
    }

    // 長さ１のチェーン
    assertPatchChain("ver1.0.0", Set(), Set(), List("ver1.0.0"))
    // 長さ１のチェーン　ゴミあり
    assertPatchChain("ver1.0.0", Set("foo"), Set("bar"), List("ver1.0.0"))
    // 長さ２のチェーン
    assertPatchChain(
      "patch_from_ver1.0.0_to_ver1.0.1", Set("ver1.0.0"), Set(),
      List("patch_from_ver1.0.0_to_ver1.0.1", "ver1.0.0"))
    // 長さ３のチェーン
    assertPatchChain(
      "patch_from_ver1.0.1_to_ver1.1.0",
      Set("patch_from_ver1.0.0_to_ver1.0.1", "ver1.0.0"), Set(),
      List(
        "patch_from_ver1.0.1_to_ver1.1.0", "patch_from_ver1.0.0_to_ver1.0.1",
        "ver1.0.0"))
    // 長さ３のチェーン　ゴミあり
    assertPatchChain(
      "patch_from_ver1.1.0_to_ver1.2.0",
      Set("patch_from_ver1.0.0_to_ver1.1.0", "ver1.0.0", "foo"),
      Set("bar", "buz"),
      List(
        "patch_from_ver1.1.0_to_ver1.2.0", "patch_from_ver1.0.0_to_ver1.1.0",
        "ver1.0.0"))
    // 長さ３のチェーン　別のチェーン候補
    assertPatchChain(
      "patch_from_ver1.1.0_to_ver1.2.0",
      Set(
        // つながらないチェーン
        "patch_from_ver1.0.5_to_ver1.1.0", "patch_from_ver1.0.2_to_ver1.0.5",
        // つながるチェーン
        "patch_from_ver1.0.3_to_ver1.1.0", "ver1.0.3",
        // つながらないチェーン
        "patch_from_ver1.0.2_to_ver1.1.0", "patch_from_ver1.0.1_to_ver1.0.2"),
      Set(),
      List(
        "patch_from_ver1.1.0_to_ver1.2.0", "patch_from_ver1.0.3_to_ver1.1.0",
        "ver1.0.3"))
  }

  "IOUtils" should "パッチチェーンが取り得なければInvalidPatchList例外" in {
    def assertInvalidPatchChain(
        verOrPatchDirName: String, otherDirNames: Set[String]) {
      for (parentDir <- pathTailMap.keys) {
        Atmosphere.withTestIOUtils(
            parentDir, Paths.get(verOrPatchDirName, "game.jar")) {
          agent.LoanAgent.loan {
            Files.createDirectories(
              Atmosphere.ioUtils.gameDirPath.resolve(verOrPatchDirName))
            for (dirName <- otherDirNames) {
              val path = Atmosphere.ioUtils.gameDirPath.resolve(dirName)
              Files.createDirectories(path)
            }

            val e = intercept[outerspace.IOUtils.InvalidPatchList] {
              Atmosphere.ioUtils.appliedPatchDirList
            }
            val expected = Atmosphere.ioUtils.gameDirPath.resolve(
              verOrPatchDirName).toAbsolutePath
            assert(e.path === expected)
          }
        }
      }
    }

    assertInvalidPatchChain("patch_from_ver1.1.0_to_ver1.2.0", Set("ver1.2.0"))
    assertInvalidPatchChain(
      "patch_from_ver1.1.0_to_ver1.2.0",
      Set("patch_from_ver1.0.0_to_ver1.0.1", "ver1.0.0"))
    assertInvalidPatchChain("foo", Set())
  }

  "IOUtils" should "パッチチェーン内で最新のファイルを選択" in {
    for (parentDir <- pathTailMap.keys; patchableJPath <- pathTailMap.keys) {
      var patchablePath = patchableJPath.getName(0).toString
      for (i <- 1 until patchableJPath.getNameCount()) {
        // OSによらず、スラッシュ区切りの文字列
        patchablePath += s"/${patchableJPath.getName(i).toString}"
      }

      Atmosphere.withTestIOUtils(
          parentDir,
          Paths.get("patch_from_ver1.0.1_to_ver1.1.0", "game.jar")) {
        agent.LoanAgent.loan {
          Files.createDirectories(Atmosphere.ioUtils.gameDirPath.resolve(
            "patch_from_ver1.0.1_to_ver1.1.0"))
          Files.createDirectories(Atmosphere.ioUtils.gameDirPath.resolve(
            "patch_from_ver1.0.0_to_ver1.0.1"))
          Files.createDirectories(Atmosphere.ioUtils.gameDirPath.resolve(
            "ver1.0.0"))

          intercept[outerspace.IOUtils.FileIsNotFound] {
            Atmosphere.ioUtils.searchPatchedFile(patchablePath)
          }

          val oldFilePath = Atmosphere.ioUtils.gameDirPath.resolve(
            "ver1.0.0").resolve(patchableJPath)
          Files.createDirectories(oldFilePath.getParent())
          Files.createFile(oldFilePath)
          assert(Atmosphere.ioUtils.searchPatchedFile(patchablePath) ===
                 oldFilePath)

          val newerFilePath = Atmosphere.ioUtils.gameDirPath.resolve(
            "patch_from_ver1.0.0_to_ver1.0.1").resolve(patchableJPath)
          Files.createDirectories(newerFilePath.getParent())
          Files.createFile(newerFilePath)
          assert(Atmosphere.ioUtils.searchPatchedFile(patchablePath) ===
                 newerFilePath)

          val newestFilePath = Atmosphere.ioUtils.gameDirPath.resolve(
            "patch_from_ver1.0.1_to_ver1.1.0").resolve(patchableJPath)
          Files.createDirectories(newestFilePath.getParent())
          Files.createFile(newestFilePath)
          assert(Atmosphere.ioUtils.searchPatchedFile(patchablePath) ===
                 newestFilePath)
        }
      }
    }
  }
}
