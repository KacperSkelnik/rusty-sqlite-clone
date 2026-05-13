use crate::page::{Page, PageId};
use std::collections::HashMap;
use std::fs::File;

pub enum PagerError {
    PageNotFound,
}

pub trait Pager {
    fn get_page(&self, page_id: PageId) -> Result<&Page, PagerError>;
    fn alloc_page(&mut self) -> Result<PageId, PagerError>;
    fn write_page(&mut self, page_id: PageId, data: &Page) -> Result<(), PagerError>;
    fn free_page(&mut self, page_id: PageId) -> Result<(), PagerError>;
}

// For testing / in-memory use
pub struct MemoryPager {
    pages: HashMap<PageId, Page>,
}

impl Pager for MemoryPager {
    fn get_page(&self, page_id: PageId) -> Result<&Page, PagerError> {
        self.pages.get(&page_id).ok_or(PagerError::PageNotFound)
    }

    fn alloc_page(&mut self) -> Result<PageId, PagerError> {
        let page_id = self.pages.len() as PageId;
        self.pages.insert(page_id, Page::empty(page_id));
        Ok(page_id)
    }

    fn write_page(&mut self, page_id: PageId, data: &Page) -> Result<(), PagerError> {
        self.pages.insert(page_id, data.clone());
        Ok(())
    }

    fn free_page(&mut self, page_id: PageId) -> Result<(), PagerError> {
        self.pages.remove(&page_id);
        Ok(())
    }
}

// For production / persistence
pub struct FilePager {
    file: File,
    page_size: usize,
    cache: MemoryPager,
}
