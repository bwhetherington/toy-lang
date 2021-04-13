let iter = new Range(0, 10)
let flat = iter.flat_map((x) => [1, 2, x])
println([...flat])