pub type PageId = u64;

//Raw data structure to store BTreeNode
#[derive(Clone, Debug)]
pub struct Page {
    pub id: PageId,
    pub data: Vec<u8>,
}

impl Page {
    pub fn empty(id: PageId) -> Page {
        Page { id, data: vec![] }
    }
}
