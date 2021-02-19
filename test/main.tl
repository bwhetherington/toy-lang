let util = import("./util/index.tl");

pub func main() {
  let list = [1, 2, 3, 4];
  print(list);
  print(util.square_list(list));
}