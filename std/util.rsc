pub fn fib(n) {
  if n < 2 {
    return n;
  } else {
    return fib(n - 1) + fib(n - 2);
  }
}

pub let Box = class.define({
  init(val) {
    self._val = val;
  },
  get() {
    return self._val;
  },
  set(val) {
    self._val = val;
  },
});