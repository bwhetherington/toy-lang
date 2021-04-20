let foo = {
  bar(f) {
    f()
  }
}

let f = () => println(self)
foo.bar(f)