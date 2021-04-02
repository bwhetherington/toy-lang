pub let curry = (f, ...curried) => (...args) => f(...curried, ...args)

Function.curry = (...curried) => (...args) => self(...curried, ...args)