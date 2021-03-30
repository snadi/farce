package gsd.commons

import org.slf4j.LoggerFactory

/**
 * Created by IntelliJ IDEA.
 * User: berger
 * Date: 17.06.2010
 * Time: 12:34:41
 * To change this template use File | Settings | File Templates.
 */
trait Logging{

  private val log = LoggerFactory.getLogger(getClass)
	def ltrace(message: =>String, values:Object*) = if( log.isTraceEnabled ) log.trace(message, values)
	def ltrace(message: =>String, error:Throwable) = if( log.isTraceEnabled ) log.trace(message, error)
	def ltrace(message: =>String, error:Throwable, values:Object*) =
		if( log.isTraceEnabled ) log.trace(message, error, values)

	def ldebug(message: =>String, values:Object*) = if( log.isDebugEnabled ) log.debug(message, values)
	def ldebug(message: =>String, error:Throwable) = if( log.isDebugEnabled ) log.debug(message, error)
	def ldebug(message: =>String, error:Throwable, values:Object*) =
		if( log.isDebugEnabled ) log.debug(message, error, values)

	def linfo(message: =>String, values:Object*) = if( log.isInfoEnabled ) log.info(message, values)
	def linfo(message: =>String, error:Throwable) = if( log.isInfoEnabled ) log.info(message, error)
	def linfo(message: =>String, error:Throwable, values:Object*) =
		if( log.isInfoEnabled ) log.info(message, error, values)

	def lwarn(message: =>String, values:Object*) = if( log.isWarnEnabled ) log.warn(message, values)
	def lwarn(message: =>String, error:Throwable) = if( log.isWarnEnabled ) log.warn(message, error)
	def lwarn(message: =>String, error:Throwable, values:Object*) =
		if( log.isWarnEnabled ) log.warn(message, error, values)

	def lerror(message: =>String, values:Object*) = if( log.isErrorEnabled ) log.error(message, values)
	def lerror(message: =>String, error:Throwable) = if( log.isErrorEnabled ) log.error(message, error)
	def lerror(message: =>String, error:Throwable, values:Object) =
		if( log.isErrorEnabled ) log.error(message, error, values)

}