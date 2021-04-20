let A = 1664525;
let C = 1013904223;
let M = 4294967296;

pub class Rng : Iterator {
  init(seed) {
    self._current = seed % M
    self.next()
  }

  next() {
    self._current = (A * self._current + C) % M
    return self._current / M
  }

  ints(min, max) {
    return self.map((r) => floor(r * (max - min)) + min)
  }
}

pub fn rng() {
  return new Rng(time())
}

let GLOBAL_RNG = rng()

pub fn random() {
  return GLOBAL_RNG.next()
}

pub fn rand_int(low, high) {
  return GLOBAL_RNG.ints(low, high).next()
}

fn max(a, b) {
    if b > a {
        return b
    } else {
        return a
    }
}

fn min(a, b) {
  if b < a {
    return b
  } else {
    return a
  }
}

pub fn advantage_rolls() {
  let seed = time()
  let a = new Rng(seed).ints(1, 21)
  let b = new Rng(seed + 1).ints(1, 2)
  return a.zip_with(b, max)
}

pub fn disadvantage_rolls() {
  let seed = time()
  let a = new Rng(seed).ints(1, 21)
  let b = new Rng(seed + 1).ints(1, 21)
  return a.zip_with(b, min)
}