pub class Maybe {
  map(f) {
    return None
  }

  flat_map(f) {
    return None
  }
}

pub class Just : Maybe {
  init(val) {
    super.init()
    self._val = val
  }

  map(f) {
    return new Just(f(self._val))
  }

  flat_map(f) {
    return f(self._val)
  }
}

pub let Nothing = new Maybe()