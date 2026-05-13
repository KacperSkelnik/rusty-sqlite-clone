use crate::btreenode::{BTreeNode, InternalNode, LeafNode};
use crate::page::PageId;
use crate::pager::{Pager, PagerError};
use crate::serializer::{Serializer, SerializerError};
use std::marker::PhantomData;

//Suppose the order of a B-tree is N. It means every node can have a maximum of N children.
//Therefore, every node can have maximum (N-1) keys and minimum (N/2)-1 keys except the root node.
pub enum BTreeError {
    KeyNotFound,
    TheTreeIsCorrupted,
}

impl From<PagerError> for BTreeError {
    fn from(_: PagerError) -> Self {
        todo!()
    }
}

impl From<SerializerError> for BTreeError {
    fn from(_: SerializerError) -> Self {
        todo!()
    }
}

pub struct BTree<P: Pager, K, V, S: Serializer<K, V>> {
    pager: P,
    serializer: S,
    root_page_id: PageId,
    n: usize,
    _marker: PhantomData<(K, V)>,
}

impl<P: Pager, K, V, S: Serializer<K, V>> BTree<P, K, V, S>
where
    K: Ord + Copy,
    V: Clone,
{
    pub fn store(&mut self, key: K, value: V) -> Result<(), BTreeError> {
        let leaf = self.find_leaf(key)?;
        if leaf.keys.len() >= self.n - 1 {
            self.split_leaf(leaf)?;
            let leaf = self.find_leaf(key)?; // re-find after split since the tree structure changed
            self.insert_into_leaf(leaf, key, value)
        } else {
            self.insert_into_leaf(leaf, key, value)
        }
    }

    pub fn search(&self, key: K) -> Result<V, BTreeError> {
        let leaf = self.find_leaf(key)?;
        for (i, k) in leaf.keys.iter().enumerate() {
            if key == *k {
                return Ok(leaf.values[i].clone());
            }
        }
        Err(BTreeError::KeyNotFound)
    }

    pub fn delete(&mut self, key: K) -> Result<(), BTreeError> {
        let leaf = self.find_leaf(key)?;
        todo!()
    }

    fn find_leaf(&self, key: K) -> Result<LeafNode<K, V>, BTreeError> {
        let mut next_page_id = self.root_page_id;
        let mut parents = vec![];
        loop {
            let page = self.pager.get_page(next_page_id).map_err(BTreeError::from)?;
            let node = S::deserialize_to_node(page, &parents).map_err(BTreeError::from)?;
            match node {
                BTreeNode::Leaf(leaf) => return Ok(leaf),
                BTreeNode::Internal(InternalNode { page_id, keys, children, .. }) => {
                    // keys: [ k0, k1, k2 ]
                    // children: [ c0, c1, c2, c3 ]
                    // Where c0 holds keys < k0, c1 holds k0 <= keys < k1, etc.
                    // So the rule is: follow children[i] where i is the first index where key < keys[i], and if no such key exists, follow the last child.
                    let child_index = keys.partition_point(|k| *k < key);
                    next_page_id = children[child_index];
                    parents.push(page_id);
                }
            }
        }
    }

    fn insert_into_leaf(&mut self, mut leaf: LeafNode<K, V>, key: K, value: V) -> Result<(), BTreeError> {
        let new_index = leaf.keys.partition_point(|k| *k < key);
        leaf.keys.insert(new_index, key);
        leaf.values.insert(new_index, value);
        self.insert_node(leaf.page_id, BTreeNode::Leaf(leaf))
    }

    fn insert_into_internal(
        &mut self,
        mut internal: InternalNode<K>,
        key: K,
        right_child: PageId,
    ) -> Result<(), BTreeError> {
        let new_index = internal.keys.partition_point(|k| *k < key);
        internal.keys.insert(new_index, key);
        internal.children.insert(new_index + 1, right_child);
        self.insert_node(internal.page_id, BTreeNode::Internal(internal))
    }

    fn insert_node(&mut self, page_id: PageId, node: BTreeNode<K, V>) -> Result<(), BTreeError> {
        let page = S::serialize_to_page(&node).map_err(BTreeError::from)?;
        self.pager.write_page(page_id, &page).map_err(BTreeError::from)
    }

    fn split_leaf(&mut self, node: LeafNode<K, V>) -> Result<(), BTreeError> {
        let mut left = node;

        // split keys/values at mid = len / 2
        let mid = left.keys.len() / 2;

        // alloc a new page for the right half
        let right_page_id = self.pager.alloc_page().map_err(BTreeError::from)?;

        // left page keeps [0...mid]
        // right page keeps [mid...end]
        let right_keys = left.keys.split_off(mid);
        let right_values = left.values.split_off(mid);
        let mut right: LeafNode<K, V> = BTreeNode::empty_leaf(right_page_id, left.parents.clone());
        right.keys = right_keys;
        right.values = right_values;

        // set left.next_leaf = new right page_id
        left.next_leaf = Some(right_page_id);

        // save the parents for later
        let parents = left.parents.clone();

        // save the middle key for later
        let key_to_promote = right.keys[0];

        // serialize and write left back to original page_id
        self.insert_node(left.page_id, BTreeNode::Leaf(left))?;

        // serialize and write right to new page_id
        let right_page_id = right.page_id;
        self.insert_node(right_page_id, BTreeNode::Leaf(right))?;

        // promote the middle key to parent
        if let Some(parent_page_id) = parents.last() {
            let page = self.pager.get_page(*parent_page_id)?;
            let sub_parents = parents[..parents.len() - 1].to_vec();
            match S::deserialize_to_node(page, &sub_parents)? {
                BTreeNode::Internal(internal) => {
                    if internal.keys.len() >= self.n - 1 {
                        self.split_internal(internal)?;
                    } else {
                        self.insert_into_internal(internal, key_to_promote, right_page_id)?;
                    }
                }
                _ => return Err(BTreeError::TheTreeIsCorrupted),
            };
        }

        Ok(())
    }

    fn split_internal(&mut self, node: InternalNode<K>) -> Result<(), BTreeError> {
        todo!()
    }
}
