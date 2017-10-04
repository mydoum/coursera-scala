Range(1, 9)

val data = Array[Float](0f, 1f, 8f, 9f)
val from = 4
val until = 5
val res: Array[(Float, Float)] = data.slice(0, data.length).zip(Range(from, until).map(el => el.toFloat))
val computed = res.foldLeft(0f: Float)((acc, v) => 0f)

