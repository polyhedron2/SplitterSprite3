package jp.gr.java_conf.polyhedron.splittersprite3.outerspace

import java.awt.{Toolkit, Dimension, Graphics2D}
import java.awt.event.{KeyListener, MouseListener, KeyEvent, MouseEvent}
import javax.swing.{JFrame, JPanel}

import jp.gr.java_conf.polyhedron.splittersprite3.{Atmosphere}

class Window(gameName: String, mode: String, val width: Int, val height: Int) {
  private val bufferNum = 2

  private val gameTitle = if (mode == "main") {
    s"${gameName} ${Atmosphere.ioUtils.versionName}"
  } else {
    s"${gameName} ${Atmosphere.ioUtils.versionName} MODE=${mode}"
  }

  private val frame = new JFrame(gameTitle)
  frame.setBounds(0, 0, width, height)
  frame.setResizable(false)
  frame.setIgnoreRepaint(true)
  frame.setVisible(true)
  frame.createBufferStrategy(bufferNum)
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

  private val panel = new JPanel() with KeyListener with MouseListener {
    // TODO: Mouse系の実装
    def mouseClicked(e: MouseEvent) { }
    def mouseEntered(e: MouseEvent) { }
    def mouseExited(e: MouseEvent) { }
    def mousePressed(e: MouseEvent) { }
    def mouseReleased(e: MouseEvent) { }

    def keyTyped(e: KeyEvent) { }
    def keyPressed(e: KeyEvent) {
      Atmosphere.commandQueue.enqueuePress(e.getKeyCode())
    }
    def keyReleased(e: KeyEvent) {
      Atmosphere.commandQueue.enqueueRelease(e.getKeyCode())
    }
  }
  panel.setPreferredSize(new Dimension(width, height))
  panel.setSize(width, height)

  frame.getContentPane().add(panel)
  frame.addMouseListener(panel)
  frame.addKeyListener(panel)

  frame.pack()

  def ensureBufferStrategy() {
    def tryOnce() {
      val bufferStrategy = frame.getBufferStrategy()
      if (bufferStrategy.contentsLost()) {
        frame.createBufferStrategy(bufferNum)
        tryOnce()
      } else {
        bufferStrategy.show()
      }
    }
    tryOnce()
    Toolkit.getDefaultToolkit().sync()
  }

  def graphics: Graphics2D = {
    val bufferStrategy = frame.getBufferStrategy()
    val g = bufferStrategy.getDrawGraphics()
    val insets = frame.getInsets()
    g.translate(insets.left, insets.top)
    g.asInstanceOf[Graphics2D]
  }

  def showFPS(fps: Double) {
    val titleWithFPS = f"${gameTitle}%s [FPS:${fps}%.2f]"
    frame.setTitle(titleWithFPS)
  }
}
