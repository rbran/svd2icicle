use std::ops::Range;

use svd_parser::svd::Peripheral;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MemoryPage {
    pub addr: u64,
    pub chunks: Vec<MemoryChunk>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MemoryChunk(pub Range<u64>);

/// create a page-mapped list of peripherals
/// TODO check that each page is aligned
pub fn memory_pages_from_chunks<'a>(
    addr_bits: u32,
    peripherals: &[Peripheral],
) -> Vec<MemoryPage> {
    let page_mask = (u64::MAX >> addr_bits) << addr_bits;
    let mut chunks = Vec::new();
    for per in peripherals.iter() {
        for reg in per.all_registers() {
            let start = per.base_address + reg.address_offset as u64;
            let bits = reg
                .properties
                .size
                .or(per.default_register_properties.size)
                .unwrap();
            match reg {
                svd_parser::svd::MaybeArray::Single(_reg) => {
                    let end = start + (bits as u64 / 8);
                    // don't allow register between pages
                    assert_eq!(
                        start & page_mask,
                        (end - 1) & page_mask,
                        "reg start {start} end {end}"
                    );
                    chunks.push(MemoryChunk(start..end))
                }
                svd_parser::svd::MaybeArray::Array(_reg, dim) => {
                    let mut start = start;
                    for _ in 0..dim.dim {
                        let end = start + (bits as u64 / 8);
                        chunks.push(MemoryChunk(start..end));

                        start += dim.dim_increment as u64;
                    }
                }
            }
        }
    }
    // sort chunks so lowest address comes first, also but biggest first to
    // optimize the algorithm below.
    chunks.sort_unstable_by(|a, b| match a.0.start.cmp(&b.0.start) {
        std::cmp::Ordering::Equal => b.0.end.cmp(&a.0.end),
        x => x,
    });

    // combine multiple chunks into pages
    chunks.into_iter().fold(vec![], |mut pages, chunk| {
        // if overlapping with the last page, just accumulate. Otherwise
        // start a new page
        let chunk_page = chunk.0.start & page_mask;
        match pages.last_mut() {
            // if it belongs to the same page, add this chunk to the last page
            Some(last) if last.addr == chunk_page => {
                // merge the chunks if they are intersecting/sequential
                match last.chunks.last_mut() {
                    Some(last) if last.0.end >= chunk.0.start => {
                        last.0.end = last.0.end.max(chunk.0.end)
                    }
                    _ => last.chunks.push(chunk),
                }
            }
            // start a new page
            _ => pages.push(MemoryPage {
                addr: chunk_page,
                chunks: vec![chunk],
            }),
        }
        pages
    })
}
