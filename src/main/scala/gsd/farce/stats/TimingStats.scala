package gsd.farce.stats

/**
 * Created with IntelliJ IDEA.
 * User: snadi
 * Date: 20/08/13
 * Time: 1:50 AM
 * To change this template use File | Settings | File Templates.
 */
class TimingStats(lextingTime: Int, parsingTime: Int, typeTime: Int, symbolCreationTime: Int) {

  def getLexingTime = lextingTime
  def getParsingTime = parsingTime
  def getTypeTime = typeTime
  def getSymbolCreationTime = symbolCreationTime

}
