class Box {
  init(val) {
    self._val = val;
  }

  get() {
    return self._val;
  }

  set(val) {
    self._val = val;
  }
}

class IncBox : Box {
  init(val, inc) {
    super.init(val);
    self._inc = inc;
  }

  get() {
    return super.get() + self._inc;
  }
}

let x = IncBox.new(10, 5);
print(x.get());
x.set(20);
print(x.get());