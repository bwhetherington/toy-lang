Number.plus = (other) => other.plus(self)
Number.multiply = (other) => other.multiply(self)
Function.curry = (...curried) => {
  let f = self
  return (...args) => f(...curried, ...args)
}

List.sum = () => self.iter().fold(0, (sum, x) => sum + x)
List.product = () => self.iter().fold(1, (product, x) => product * x)