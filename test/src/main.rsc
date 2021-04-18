let foo = {
  bar(f) {
    f()
  }
}

println(self)

let f = () => println(self)
foo.bar(f)