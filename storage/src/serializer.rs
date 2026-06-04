use crate::btreenode::{BTreeNode, InternalNode, LeafNode};
use crate::page::{Page, PageId};

pub enum SerializerError {}

pub trait Serializer<K, V> {
    fn serialize_to_page_internal(node: &InternalNode<K>) -> Result<Page, SerializerError>;
    fn serialize_to_page_leaf(node: &LeafNode<K, V>) -> Result<Page, SerializerError>;
    fn deserialize_to_node(page: &Page, parents: &Vec<PageId>) -> Result<BTreeNode<K, V>, SerializerError>;
}

pub struct DefaultSerializer;

impl Serializer<u32, Vec<u8>> for DefaultSerializer {
    fn serialize_to_page_internal(node: &InternalNode<u32>) -> Result<Page, SerializerError> {
        Ok(Page::empty(node.page_id))
    }

    fn serialize_to_page_leaf(node: &LeafNode<u32, Vec<u8>>) -> Result<Page, SerializerError> {
        Ok(Page::empty(node.page_id))
    }

    fn deserialize_to_node(page: &Page, parents: &Vec<PageId>) -> Result<BTreeNode<u32, Vec<u8>>, SerializerError> {
        todo!()
    }
}
