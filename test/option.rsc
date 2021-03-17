pub class Maybe {
  map(f) {
    return None;
  }

  flat_map(f) {
    return None;
  }
}

pub class Just : Maybe {
  init(val) {
    super.init();
    self._val = val;
  }

  map(f) {
    return Just.new(f(self._val));
  }

  flat_map(f) {
    return f(self._val);
  }
}

pub let Nothing = Maybe.new();

List.get = (i) => {
  if (0 <= i) && (i < self.len()) {
    return Just.new(self[i]);
  } else {
    return Nothing;
  }
};

[1, 2, 3]
  .get(2)
  .map((x) => {
    println(x);
  });