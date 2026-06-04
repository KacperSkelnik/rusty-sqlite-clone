use crate::btreenode::{BTreeNode, InternalNode, LeafNode};
use crate::page::{Page, PageId};
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
        let mut leaf = self.find_leaf(key)?;
        if leaf.keys.len() >= self.n - 1 {
            self.split_leaf(&mut leaf)?;
            let mut rebalanced_leaf = self.find_leaf(key)?; // re-find after split since the tree structure changed
            self.insert_into_leaf(&mut rebalanced_leaf, key, value)
        } else {
            self.insert_into_leaf(&mut leaf, key, value)
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

    fn insert_into_leaf(&mut self, leaf: &mut LeafNode<K, V>, key: K, value: V) -> Result<(), BTreeError> {
        let new_index = leaf.keys.partition_point(|k| *k < key);
        leaf.keys.insert(new_index, key);
        leaf.values.insert(new_index, value);
        let page = S::serialize_to_page_leaf(leaf).map_err(BTreeError::from)?;
        self.insert_page(leaf.page_id, page)
    }

    fn insert_into_internal(
        &mut self,
        internal: &mut InternalNode<K>,
        key: K,
        right_child: PageId,
    ) -> Result<(), BTreeError> {
        let new_index = internal.keys.partition_point(|k| *k < key);
        internal.keys.insert(new_index, key);
        internal.children.insert(new_index + 1, right_child);
        let page = S::serialize_to_page_internal(internal).map_err(BTreeError::from)?;
        self.insert_page(internal.page_id, page)
    }

    fn insert_page(&mut self, page_id: PageId, page: Page) -> Result<(), BTreeError> {
        self.pager.write_page(page_id, &page).map_err(BTreeError::from)
    }

    fn split_leaf(&mut self, node: &mut LeafNode<K, V>) -> Result<(), BTreeError> {
        let left = node;

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
        let left_page = S::serialize_to_page_leaf(left).map_err(BTreeError::from)?;
        self.insert_page(left.page_id, left_page)?;

        // serialize and write right to new page_id
        let right_page_id = right.page_id;
        let right_page = S::serialize_to_page_leaf(&right).map_err(BTreeError::from)?;
        self.insert_page(right_page_id, right_page)?;

        // promote the middle key to parent
        if let Some(parent_page_id) = parents.last() {
            let page = self.pager.get_page(*parent_page_id)?;
            let sub_parents = parents[..parents.len() - 1].to_vec();
            match S::deserialize_to_node(page, &sub_parents)? {
                BTreeNode::Internal(mut internal) => {
                    if internal.keys.len() >= self.n - 1 {
                        self.split_internal(&mut internal, key_to_promote, right_page_id)?;
                    } else {
                        self.insert_into_internal(&mut internal, key_to_promote, right_page_id)?;
                    }
                }
                _ => return Err(BTreeError::TheTreeIsCorrupted),
            };
        } else {
            todo!()
        }

        Ok(())
    }

    fn split_internal(&mut self, node: &mut InternalNode<K>, key: K, right_child: PageId) -> Result<(), BTreeError> {
        // It uses loop to avoid stack overflow issues with recurrent calls
        let mut workable_node = node.clone();
        let mut workable_key = key;
        let mut workable_right_child = right_child;
        loop {
            // insert a new key that overflows the node - memory only
            let new_index = workable_node.keys.partition_point(|k| *k < workable_key);
            workable_node.keys.insert(new_index, workable_key);
            workable_node.children.insert(new_index + 1, workable_right_child);

            let left = &mut workable_node;

            // split keys/values at mid = len / 2
            let mid = left.keys.len() / 2;

            // alloc a new page for the right half
            let right_page_id = self.pager.alloc_page().map_err(BTreeError::from)?;

            // left page keeps keys [0...mid), children [0...mid+1)
            // right page keeps keys [mid+1...end), children [mid+1...end)
            // keys[mid] is promoted to parent
            let right_keys = left.keys.split_off(mid);
            let right_children = left.children.split_off(mid + 1);
            let mut right: InternalNode<K> = BTreeNode::<K, V>::empty_internal(right_page_id, left.parents.clone());
            right.keys = right_keys;
            right.children = right_children;

            // save the parents for later
            let parents = left.parents.clone();

            // save the middle key for later
            let key_to_promote = right.keys.remove(0);

            // serialize and write left back to original page_id
            let left_page = S::serialize_to_page_internal(left).map_err(BTreeError::from)?;
            self.insert_page(left.page_id, left_page)?;

            // serialize and write right to new page_id
            let right_page_id = right.page_id;
            let right_page = S::serialize_to_page_internal(&right).map_err(BTreeError::from)?;
            self.insert_page(right_page_id, right_page)?;

            // promote the middle key to parent
            if let Some(parent_page_id) = parents.last() {
                let page = self.pager.get_page(*parent_page_id)?;
                let sub_parents = parents[..parents.len() - 1].to_vec();
                match S::deserialize_to_node(page, &sub_parents)? {
                    BTreeNode::Internal(mut internal) => {
                        if internal.keys.len() >= self.n - 1 {
                            workable_node = internal;
                            workable_key = key_to_promote;
                            workable_right_child = right_page_id;
                        } else {
                            self.insert_into_internal(&mut internal, key_to_promote, right_page_id)?;
                            break;
                        }
                    }
                    _ => return Err(BTreeError::TheTreeIsCorrupted),
                };
            } else {
                todo!()
            }
        }

        Ok(())
    }
}
