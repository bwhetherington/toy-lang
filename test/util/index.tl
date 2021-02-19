let iter = import("std/iter.tl");

pub func square_list(list) {
  let new_list = [];
  let i = iter.list(list)
    .map((x) => x^2);
  for x in i {
    new_list.push(x);
  }
  return new_list;
}