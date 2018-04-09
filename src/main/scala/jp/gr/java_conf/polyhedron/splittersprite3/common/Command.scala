package jp.gr.java_conf.polyhedron.splittersprite3.common

sealed abstract class Command
case class KeyPress(text: String) extends Command
case class KeyRelease(text: String) extends Command
