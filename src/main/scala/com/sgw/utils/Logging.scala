package com.sgw.utils

import org.slf4j.{Logger, LoggerFactory}

trait Logging {
  private[this] final lazy val _logger: Logger = LoggerFactory.getLogger(getClass)

  def debug(msg: => Any): Unit = _logger.debug(msg.toString)

  def info(msg: => Any): Unit = _logger.info(msg.toString)

  def warn(msg: => Any): Unit = _logger.warn(msg.toString)
  def warn(msg: => Any, ex: Throwable): Unit = _logger.warn(msg.toString, ex)

  def error(msg: => Any): Unit = _logger.error(msg.toString)
  def error(msg: => Any, ex: Throwable): Unit = _logger.error(msg.toString, ex)
}
