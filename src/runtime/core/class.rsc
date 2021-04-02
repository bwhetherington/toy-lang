let ClassCore = {
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

pub fn __define_class__(Class) {
  return create_object(ClassCore, Class)
}

pub fn __extend_class__(Base, Class) {
  return __define_class__(create_object(Class, {
    create() {
      let base_instance = create_object(Base, Class)
      return create_object(base_instance, {})
    },
  }))
}