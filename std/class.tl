pub let create_class = (Base) => create_object({
  create: () => {
    let obj = create_object(Base, {});
    return obj;
  },
  new: (...args) => {
    let instance = self.create();
    instance.init(...args);
    return instance;
  },
}, Base);

pub let extend_class = (Base, Class) => create_object(Class, {
  create: () => {
    let base_obj = Base.create();
    let next = create_object(base_obj, Class);
    let instance = create_object(next, {});
    return instance;
  },
  new: (...args) => {
    let instance = self.create();
    instance.init(...args);
    return instance;
  },
});