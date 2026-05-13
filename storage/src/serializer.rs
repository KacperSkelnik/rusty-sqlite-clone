use crate::btreenode::{BTreeNode, InternalNode, LeafNode};
use crate::page::{Page, PageId};

pub enum SerializerError {}

pub trait Serializer<K, V> {
    fn serialize_to_page(node: &BTreeNode<K, V>) -> Result<Page, SerializerError>;
    fn deserialize_to_node(page: &Page, parents: &Vec<PageId>) -> Result<BTreeNode<K, V>, SerializerError>;
}

pub struct DefaultSerializer;

impl Serializer<u32, Vec<u8>> for DefaultSerializer {
    fn serialize_to_page(node: &BTreeNode<u32, Vec<u8>>) -> Result<Page, SerializerError> {
        match node {
            BTreeNode::Leaf(LeafNode { page_id, .. }) => Ok(Page::empty(*page_id)),
            BTreeNode::Internal(InternalNode { page_id, .. }) => Ok(Page::empty(*page_id)),
        }
    }

    fn deserialize_to_node(page: &Page, parents: &Vec<PageId>) -> Result<BTreeNode<u32, Vec<u8>>, SerializerError> {
        todo!()
    }
}
