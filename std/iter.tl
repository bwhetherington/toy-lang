let class = import("./class.tl");

pub let Iterator = class.define({
  next: () => None,
  iter: () => self,
});

pub let Range = class.extend(Iterator, {
  init: (from, to) => {
    super.init();
    self._from = from;
    self._to = to;
  },
  next: () => {
    if self._from < self._to {
      let i = self._from;
      self._from += 1;
      return i;
    }
  },
});

let Filter = class.extend(Iterator, {
  init: (base, pred) => {
    self._base = base;
    self._pred = pred;
  },
  next: () => {
    let val = self._base.next();
    while !((val == None) || self._pred(val)) {
      val = self._base.next();
    }
    return val;
  }
});

let Map = class.extend(Iterator, {
  init: (base, fn) => {
    super.init();
    self._base = base;
    self._fn = fn;
  },
  next: () => {
    let val = self._base.next();
    if val != None {
      return self._fn(val);
    }
  },
});

Iterator.filter = (f) => Filter.new(self, f);
Iterator.map = (f) => Map.new(self, f);

pub let list = (list) => Range.new(0, list.len()).map((i) => list[i]);