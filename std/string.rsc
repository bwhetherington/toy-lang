pub class StringBuilder {
  init() {
    self._parts = []
  }

  push(part) {
    self._parts.push(part)
  }

  pop() {
    return self._parts.pop()
  }

  str() {
    return self._parts.iter().fold("", (sum, x) => sum + x)
  }
}