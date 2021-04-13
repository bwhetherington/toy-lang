// println
pub fn as_str(x) {
  // Call the `x.str()` if the method exists
  return x?.str?.() ?? ("" + x)
}

pub fn print(...xs) {
  let strs = xs.iter().map(as_str)
  __print__(...xs)
}

pub fn println(...xs) {
  print(...xs)
  __print__("\n")
}