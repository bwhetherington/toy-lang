pub class Iterator {
  next() {
    return None;
  }

  iter() {
    return self;
  }

  for_each(f) {
    for x in self {
      f(x);
    }
  }

  collect() {
    return [...self];
  }

  fold(initial, accumulator) {
    let val = initial;
    for x in self {
      val = accumulator(val, x);
    }
    return val;
  }
}

pub class Range : Iterator {
  init(from, to) {
    super.init();
    self._from = from;
    self._to = to;
  }
  
  next() {
    if self._from < self._to {
      let i = self._from;
      self._from += 1;
      return i;
    }
  }
}

class Filter : Iterator {
  init(base, pred) {
    self._base = base;
    self._pred = pred;
  }

  next() {
    let val = self._base.next();
    let cond = !((val == None) || self._pred(val));
    while cond {
      val = self._base.next();
    }
    return val;
  }
}

class Map : Iterator {
  init(base, f) {
    super.init();
    self._base = base;
    self._f = f;
  }

  next() {
    let val = self._base.next();
    if val != None {
      return self._f(val);
    }
  }
}

class Skip : Iterator {
  init(base, amount) {
    super.init();
    self._base = base;
    self._amount = amount;
    self._current = 0;
  }

  next() {
    let val = self._base.next();
    if self._current < self._amount {
      self._current += 1;
    } else {
      return val;
    }
  }
}

class Enumerate : Iterator {
  init(base) {
    super.init();
    self._base = base;
    self._index = 0;
  }

  next() {
    let val = self._base.next();
    if val != None {
      let index = self._index;
      self._index += 1;
      return {
        value: val,
        index: index,
      };
    }
  }
}

Iterator.filter = (f) => Filter.new(self, f);
Iterator.map = (f) => Map.new(self, f);
Iterator.skip = (amount) => Skip.new(self, amount);
Iterator.enumerate = () => Enumerate.new(self);

fn list_iter(xs) {
  return Range.new(0, xs.len()).map((i) => xs[i]);
}

List.iter = () => {
  println("self", self);
  return list_iter(self);
};