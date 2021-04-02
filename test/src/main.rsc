let Matrix = import("./vec.rsc").Matrix

let id = Matrix.data([
  [1, 0, 0],
  [0, 1, 0],
  [0, 0, 1],
])

let translate = Matrix.data([
  [1, 0, 2],
  [0, 1, 3],
  [0, 0, 1],
])

let v = Matrix.data([
  [2],
  [2],
  [1],
])