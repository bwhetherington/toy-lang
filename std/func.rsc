let curry = (f, ...curried) => (...args) => f(...curried, ...args)

Function.curry = (...curried) => curry(self, ...curried)