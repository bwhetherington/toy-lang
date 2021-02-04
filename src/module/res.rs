pub trait ResourceLoader {
    type Item;

    fn load_resource(&mut self, uri: &str) -> Option<Self::Item>;
}
