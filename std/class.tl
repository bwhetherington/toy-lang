pub let define = (Base) => create_object({
  create: () => create_object(self, {}),
  init: () => {},
  new: (...args) => {
    let instance = self.create();
    instance.init(...args);
    return instance;
  },
}, Base);

pub let extend = (Base, Class) => define(create_object(Class, {
  create: () => {
    let base_instance = create_object(Base.create(), Class);
    return create_object(base_instance, {});
  },
}));