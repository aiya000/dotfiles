local list = require('utils.list')

return list.concat({
  require('snippets.scala.scala'),
  require('snippets.scala.scaladoc'),
  require('snippets.scala.scalastyle'),
  require('snippets.scala.scalatest'),
  require('snippets.scala.wartremover'),
})