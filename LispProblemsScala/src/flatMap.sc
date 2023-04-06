//List.range(1, 5) flatMap (
//  i => List.range(1, i) map (j => (i, j))
//  )

println(List.range(1,5))
println(List.range(1,5) map (i => List.range(1, i)))
println(List.range(1,5) map (i => List.range(1, i) map (j => (i, j))))
println(List.range(1,5) flatMap (i => List.range(1, i) map (j => (i, j))))

println(for (i <- List.range(1, 5); j <- List.range(1, i)) yield (i, j))

println(List("1 2 3", "4 5").map(_.split(" ").toList))
println(List("1 2 3", "4 5").flatMap(_.split(" ").toList))
println(List("1 2 3", "4 5").flatMap(line => {val fields = line.split(" "); Some(fields(0), fields(1))}))

println(List(1,2,3).flatMap(x => List(x, x)))

println(
  """
    |aaa
    |""".stripMargin)
