pub let obj = {
  foo() {
    print("Foo");
  },
  bar() {
    print("Bar");
  },
};
let util = import("./util/index.tl");

pub fn main() {
  let list = [1, 2, 3, 4];
  print(list);
  print(util.square_list(list));
}
