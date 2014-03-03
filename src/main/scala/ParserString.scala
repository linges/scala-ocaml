package scalaocaml

/**
  * Custom String implementation to be used in the parser.
  */
class ParserString(contents: String, offset: Int, length: Int) extends CharSequence {
  def this(contents: String) = this(contents, 0, contents.length())

  override def length(): Int = length
  override def charAt(index: Int): Char = contents.charAt(index + offset)
  override def subSequence(start: Int, end: Int): CharSequence = new ParserString(contents, offset + start, end - start)
  override def toString(): String = contents.substring(offset, offset + length)
}
