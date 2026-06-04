use crate::page::PageId;

#[derive(Clone, Debug)]
pub struct LeafNode<K, V> {
    pub page_id: PageId,
    pub keys: Vec<K>,
    pub values: Vec<V>,
    pub next_leaf: Option<PageId>,
    pub parents: Vec<PageId>,
}

#[derive(Clone, Debug)]
pub struct InternalNode<K> {
    pub page_id: PageId,
    pub keys: Vec<K>,
    pub children: Vec<PageId>,
    pub parents: Vec<PageId>,
}

#[derive(Clone, Debug)]
pub enum BTreeNode<K, V> {
    Internal(InternalNode<K>),
    Leaf(LeafNode<K, V>),
}

impl<K, V> BTreeNode<K, V> {
    pub fn empty_leaf(page_id: PageId, parents: Vec<PageId>) -> LeafNode<K, V> {
        LeafNode { page_id, keys: vec![], values: vec![], next_leaf: None, parents }
    }

    pub fn empty_internal(page_id: PageId, parents: Vec<PageId>) -> InternalNode<K> {
        InternalNode { page_id, keys: vec![], children: vec![], parents }
    }
}
