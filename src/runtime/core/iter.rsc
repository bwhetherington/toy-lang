/*
 * Dependencies:
 * - class.rsc
 */
pub class Iterator {
  next() {
    return None
  }

  iter() {
    return self
  }

  for_each(f) {
    for x in self {
      f(x)
    }
  }

  collect() {
    return [...self]
  }

  fold(initial, accumulator) {
    let val = initial
    for x in self {
      val = accumulator(val, x)
    }
    return val
  }
}

pub class Range : Iterator {
  init(from, to) {
    super.init()
    self._from = from
    self._to = to
  }
  
  next() {
    if self._from < self._to {
      let i = self._from
      self._from += 1
      return i
    }
  }
}

class Filter : Iterator {
  init(base, pred) {
    self._base = base
    self._pred = pred
  }

  next() {
    let val = self._base.next()
    let cond = !((val == None) || self._pred(val))
    while cond {
      val = self._base.next()
    }
    return val
  }
}

class Map : Iterator {
  init(base, f) {
    super.init()
    self._base = base
    self._f = f
  }

  next() {
    let val = self._base.next()
    if val != None {
      return self._f(val)
    }
  }
}

class FlatMap : Iterator {
  init(base, f) {
    super.init()
    self._base = base
    self._f = f
    self._cur_iter = None
  }

  next() {
    // If we have a current subiterator
    let next = self._cur_iter?.next?.()
    if next != None {
      return next
    }

    // Otherwise create a new one
    let next_base = self._base.next()
    if next_base != None {
      let next_iter = self._f(next_base)?.iter?.()
      self._cur_iter = next_iter
      return next_iter.next()
    }
  }
}

class Skip : Iterator {
  init(base, amount) {
    super.init()
    self._base = base
    for _ in new Range(0, amount) {
      base.next()
    }
  }

  next() {
    return self._base.next()
  }
}

class Enumerate : Iterator {
  init(base) {
    super.init()
    self._base = base
    self._index = 0
  }

  next() {
    let val = self._base.next()
    if val != None {
      let index = self._index
      self._index += 1
      return {
        value: val,
        index: index,
      }
    }
  }
}

class TakeWhile : Iterator {
  init(base, f) {
    super.init()
    self._base = base
    self._f = f
    self._is_done = False
  }

  next() {
    if !self._is_done {
      let val = self._base.next()
      if self._f(val) {
        return val
      } else {
        self._is_done = True
      }
    }
  }
}

class Take : Iterator {
  init(base, num) {
    super.init()
    self._base = base
    self._num = num
  }

  next() {
    if self._num > 0 {
      self._num -= 1
      return self._base.next()
    }
  }
}

class Zip : Iterator {
  init(a, b) {
    self._a = a
    self._b = b
  }

  next() {
    let a = self._a.next()
    let b = self._b.next()
    if a != None && b != None {
      return [a, b]
    }
  }
}

class ZipWith : Iterator {
  init(a, b, f) {
    super.init()
    self._a = a
    self._b = b
    self._f = f
  }

  next() {
    let a = self._a.next()
    let b = self._b.next()
    if a != None && b != None {
      return self._f(a, b)
    }
  }
}

fn zip_vals(a, b) {
  return [a, b]
}

Iterator.filter = (f) => new Filter(self, f)
Iterator.map = (f) => new Map(self, f)
Iterator.flat_map = (f) => new FlatMap(self, f)
Iterator.skip = (amount) => new Skip(self, amount)
Iterator.enumerate = () => new Enumerate(self)
Iterator.take = (amount) => new Take(self, amount)
Iterator.take_while = (f) => new TakeWhile(self, f)
Iterator.zip_with = (b, f) => new ZipWith(self, b.iter(), f)
Iterator.zip = (b) => self.zip_with(b, zip_vals)

fn list_iter(xs) {
  return Range.new(0, xs.len()).map((i) => xs[i])
}

List.iter = () => {
  return list_iter(self)
}