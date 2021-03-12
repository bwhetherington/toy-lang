let class = import("./class.rsc");

pub let Iterator = class.define({
  next() {
    return None;
  },
  iter() {
    return self;
  },
  for_each(f) {
    for x in self {
      f(x);
    }
  },
});

pub let Range = class.extend(Iterator, {
  init(from, to) {
    super.init();
    self._from = from;
    self._to = to;
  },
  next() {
    if self._from < self._to {
      let i = self._from;
      self._from += 1;
      return i;
    }
  },
});

let Filter = class.extend(Iterator, {
  init(base, pred) {
    self._base = base;
    self._pred = pred;
  },
  next() {
    let val = self._base.next();
    let cond = !((val == None) || self._pred(val));
    while cond {
      val = self._base.next();
    }
    return val;
  }
});

let Map = class.extend(Iterator, {
  init(base, f) {
    super.init();
    self._base = base;
    self._f = f;
  },
  next() {
    let val = self._base.next();
    if val != None {
      return self._f(val);
    }
  },
});

Iterator.filter = (f) => Filter.new(self, f);
Iterator.map = (f) => Map.new(self, f);

pub fn list(xs) {
  return Range.new(0, xs.len()).map((i) => xs[i]);
}