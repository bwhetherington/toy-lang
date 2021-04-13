pub let Class = {
  create() {
    return create_object(self, {})
  },
  init() {},
  new(...args) {
    let instance = self.create()
    instance.init(...args)
    return instance
  },
}

pub fn __define_class__(NewClass) {
  return create_object(Class, NewClass)
}

pub fn __extend_class__(Base, NewClass) {
  return __define_class__(create_object(NewClass, {
    create() {
      let base_instance = create_object(Base, NewClass)
      return create_object(base_instance, {})
    },
  }))
}