//! A collection of utilities to deal with JWTs

use std::ptr;

/// DPoP functions
pub mod dpop;

#[repr(C)]
pub struct ByteArray {
    data: *mut u8,
    len: usize,
}

impl Default for ByteArray {
    fn default() -> Self {
        Self::new()
    }
}

impl ByteArray {
    pub fn from_slice(data: &mut [u8]) -> Self {
        Self {
            data: data.as_mut_ptr(),
            len: data.len(),
        }
    }

    /// # Safety
    /// Please check [`Vec::from_raw_parts`]
    pub unsafe fn to_vec(self) -> Vec<u8> {
        Vec::from_raw_parts(self.data, self.len, self.len)
    }

    pub fn new() -> Self {
        Self {
            data: ptr::null_mut(),
            len: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }
}

impl From<Vec<u8>> for ByteArray {
    fn from(mut v: Vec<u8>) -> Self {
        Self {
            data: v.as_mut_ptr(),
            len: v.len(),
        }
    }
}
