let std = import("std/mod.rsc");
let Range = std.iter.Range;

fn test(n) {
  if n % 15 == 0 {
    write_string("fizzbuzz");
  } else if n % 5 == 0 {
    write_string("buzz");
  } else if n % 3 == 0 {
    write_string("fizz");
  } else {
    write_number(n);
  }
  write_newline();
}

fn fizzbuzz(to) {
  Range.new(0, to).for_each(test);
}

pub fn main() {
  fizzbuzz(100);
}
