use std::ops::Range;

use proc_macro2::{Ident, Literal, TokenStream};
use quote::quote;
use svd_parser::svd::{
    self, ClusterInfo, MaybeArray, Name, RegisterInfo, RegisterProperties,
};

use crate::enumeration::EnumerationValues;
use crate::formater::dim_to_n;
use crate::helper::{self, Dim};
use crate::peripheral::ContextCodeGen;
use crate::register::{ClusterAccess, RegisterAccess};
use crate::PAGE_LEN;

#[derive(Debug, Clone)]
pub(crate) struct MemoryChunks<T: Clone> {
    chunks: Vec<MemoryChunk<T>>,
}

impl<T: Clone> Default for MemoryChunks<T> {
    fn default() -> Self {
        Self {
            chunks: Default::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct MemoryChunk<T: Clone> {
    things: Vec<T>,
}

impl<T: Clone> Default for MemoryChunk<T> {
    fn default() -> Self {
        Self {
            things: Default::default(),
        }
    }
}

#[derive(Debug)]
enum MemoryThingSingle<'a> {
    Register {
        properties: RegisterProperties,
        addr_offset: u32,
        register: &'a MaybeArray<RegisterInfo>,
    },
    Cluster {
        cluster: &'a MaybeArray<ClusterInfo>,
        addr_offset: u32,
        memory: MemoryChunks<MemoryThingCondensated<'a>>,
    },
}

#[derive(Clone)]
pub(crate) enum MemoryThingCondensated<'a> {
    Register {
        properties: RegisterProperties,
        addr_offset: u32,
        registers: Vec<&'a svd::Register>,
    },
    Cluster {
        clusters: Vec<&'a svd::Cluster>,
        addr_offset: u32,
        memory: MemoryChunks<MemoryThingCondensated<'a>>,
    },
}

#[derive(Debug, Clone)]
pub(crate) enum MemoryThingFinal {
    Register(RegisterAccess),
    Cluster(ClusterAccess),
}

#[derive(Debug)]
pub(crate) struct ContextMemoryGen<'a, 'b> {
    pub svds: &'a [svd::Device],
    pub enumerated_values: &'b mut Vec<EnumerationValues>,
    //pheriperals: &'b [&'a MaybeArray<PeripheralInfo>],
    pub pheriperals_name: &'b str,
    pub clusters: Vec<Vec<&'a MaybeArray<ClusterInfo>>>,
}

impl MemoryThingFinal {
    pub fn len_element(&self) -> u64 {
        match self {
            Self::Register(reg) => reg.len_element() as u64,
            Self::Cluster(clu) => clu.len_element(),
        }
    }

    pub fn len_total(&self) -> u64 {
        match self {
            Self::Register(reg) => reg.len_total(),
            Self::Cluster(clu) => clu.len_total(),
        }
    }

    pub fn offset(&self) -> u64 {
        match self {
            Self::Register(reg) => reg.address_offset as u64,
            Self::Cluster(clu) => clu.address_offset as u64,
        }
    }

    pub fn can_read(&self) -> bool {
        match self {
            Self::Register(reg) => {
                reg.properties.access.unwrap_or_default().can_read()
            }
            Self::Cluster(clu) => clu.memory.can_read(),
        }
    }

    pub fn can_write(&self) -> bool {
        match self {
            Self::Register(reg) => {
                reg.properties.access.unwrap_or_default().can_write()
            }
            Self::Cluster(clu) => clu.memory.can_write(),
        }
    }
}

impl Dim for MemoryThingFinal {
    fn array(&self) -> Option<(u32, u32)> {
        match self {
            Self::Register(reg) => reg.array(),
            Self::Cluster(clu) => clu.array(),
        }
    }
}

impl core::fmt::Debug for MemoryThingCondensated<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Register { properties, .. } => f
                .debug_tuple("Register")
                .field(&properties.size.unwrap())
                .finish(),
            Self::Cluster { memory, .. } => {
                f.debug_tuple("Cluster").field(&memory).finish()
            }
        }
    }
}

impl MemoryChunks<MemoryThingFinal> {
    pub fn len(&self) -> u64 {
        let Some(first) = self.chunks.first() else {
            return 0;
        };
        let last = self.chunks.last().unwrap();
        (last.offset() + last.len()) - first.offset()
    }
    pub fn can_read(&self) -> bool {
        self.chunks
            .iter()
            .map(|chunk| chunk.can_read())
            .reduce(|a, b| a | b)
            .unwrap_or(true)
    }
    pub fn can_write(&self) -> bool {
        self.chunks
            .iter()
            .map(|chunk| chunk.can_write())
            .reduce(|a, b| a | b)
            .unwrap_or(true)
    }

    fn offset(&self) -> u64 {
        self.chunks.first().map(|chunk| chunk.offset()).unwrap_or(0)
    }
}

impl MemoryChunk<MemoryThingFinal> {
    fn len(&self) -> u64 {
        let Some(first) = self.things.first() else {
            return 0;
        };
        let last = self.things.last().unwrap();
        (last.offset() + last.len_total()) - first.offset()
    }
    fn offset(&self) -> u64 {
        self.things.first().map(|chunk| chunk.offset()).unwrap_or(0)
    }
    pub fn can_read(&self) -> bool {
        self.things
            .iter()
            .map(|things| things.can_read())
            .reduce(|a, b| a | b)
            .unwrap_or(true)
    }
    pub fn can_write(&self) -> bool {
        self.things
            .iter()
            .map(|things| things.can_write())
            .reduce(|a, b| a | b)
            .unwrap_or(true)
    }

    fn range(&self) -> Range<u64> {
        self.offset()..self.offset() + self.len()
    }
}

impl MemoryThingSingle<'_> {
    fn len_element(&self) -> u64 {
        match self {
            Self::Register { properties, .. } => {
                properties.size.unwrap() as u64 / 8
            }
            Self::Cluster {
                memory, cluster, ..
            } => {
                if let Some((_dim, dim_len)) = cluster.array() {
                    dim_len.into()
                } else {
                    memory.len()
                }
            }
        }
    }

    fn len_total(&self) -> u64 {
        let mut len = self.len_element();
        if let Some((dim, _)) = self.array() {
            len *= dim as u64;
        }
        len
    }

    fn page_offset(&self) -> u64 {
        match self {
            Self::Register {
                addr_offset: page_offset,
                ..
            }
            | Self::Cluster {
                addr_offset: page_offset,
                ..
            } => *page_offset as u64,
        }
    }
}

impl Dim for MemoryThingSingle<'_> {
    fn array(&self) -> Option<(u32, u32)> {
        match self {
            Self::Register { register, .. } => register.array(),
            Self::Cluster { cluster, .. } => cluster.array(),
        }
    }
}

impl<'a> MemoryThingCondensated<'a> {
    fn offset(&self) -> u64 {
        match self {
            Self::Register { addr_offset, .. }
            | Self::Cluster { addr_offset, .. } => *addr_offset as u64,
        }
    }
    fn len_element(&self) -> u64 {
        match self {
            Self::Register { properties, .. } => {
                properties.size.unwrap() as u64 / 8
            }
            Self::Cluster {
                memory, clusters, ..
            } => {
                if let Some(dim) = helper::dim_from_multiple(&clusters) {
                    dim.dim_increment.into()
                } else {
                    memory.len()
                }
            }
        }
    }
    fn len_total(&self) -> u64 {
        let mut len = self.len_element();
        if let Some((dim, _)) = self.array() {
            len *= dim as u64;
        }
        len
    }
    fn page_offset(&self) -> u64 {
        match self {
            Self::Register {
                addr_offset: page_offset,
                ..
            }
            | Self::Cluster {
                addr_offset: page_offset,
                ..
            } => *page_offset as u64,
        }
    }

    fn finalize<'b>(
        self,
        context: &mut ContextMemoryGen<'a, 'b>,
    ) -> MemoryThingFinal {
        match self {
            Self::Register {
                properties,
                addr_offset: address_offset,
                registers,
            } => MemoryThingFinal::Register(RegisterAccess::new(
                context,
                properties,
                address_offset,
                registers,
            )),
            Self::Cluster {
                clusters,
                addr_offset: address_offset,
                memory,
            } => MemoryThingFinal::Cluster(ClusterAccess::new(
                context,
                clusters,
                address_offset,
                memory,
            )),
        }
    }
}

impl Dim for MemoryThingCondensated<'_> {
    fn array(&self) -> Option<(u32, u32)> {
        match self {
            Self::Register { registers, .. } => {
                helper::dim_from_multiple(&registers)
            }
            Self::Cluster { clusters, .. } => {
                helper::dim_from_multiple(&clusters)
            }
        }
        .map(|dim| (dim.dim, dim.dim_increment))
    }
}

impl<'a> From<MemoryThingSingle<'a>> for MemoryThingCondensated<'a> {
    fn from(value: MemoryThingSingle<'a>) -> Self {
        match value {
            MemoryThingSingle::Register {
                properties,
                addr_offset: address_offset,
                register,
            } => Self::Register {
                properties,
                addr_offset: address_offset,
                registers: vec![register],
            },
            MemoryThingSingle::Cluster {
                cluster,
                addr_offset: address_offset,
                memory,
            } => Self::Cluster {
                clusters: vec![cluster],
                addr_offset: address_offset,
                memory,
            },
        }
    }
}

impl MemoryChunks<MemoryThingFinal> {
    pub fn new_page<'a>(
        context: &mut ContextMemoryGen<'a, '_>,
        peripherals: &[&'a svd::Peripheral],
    ) -> Self {
        let things = peripherals
            .iter()
            .flat_map(|peripheral| {
                assert!(peripheral.derived_from.is_none());
                // combine the defaults of the cluster with the parent
                let defaults = &peripheral.default_register_properties;
                // the base address is with the page as offset
                let peripheral_offset =
                    (peripheral.base_address % PAGE_LEN) as u32;
                MemoryChunks::new_chunk(
                    defaults,
                    peripheral_offset,
                    peripheral.registers(),
                    peripheral.clusters(),
                )
            })
            .collect();
        let chunks = condensate_chunks(things);
        if chunks.len() > PAGE_LEN {
            panic!("Pheripheral Page is bigger then the page");
        }
        let chunks = chunks.finalize(context);
        chunks
    }

    pub fn gen_fields_functions<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
    ) -> TokenStream {
        self.chunks
            .iter()
            .map(|chunk| chunk.gen_fields_functions(context))
            .collect()
    }
}

impl<'a> MemoryChunks<MemoryThingCondensated<'a>> {
    fn new_chunk<'b>(
        defaults: &'b RegisterProperties,
        peripheral_offset: u32,
        registers: impl Iterator<Item = &'a svd::Register> + 'b,
        clusters: impl Iterator<Item = &'a svd::Cluster> + 'b,
    ) -> impl Iterator<Item = MemoryThingSingle<'a>> + 'b {
        let registers = registers.map(move |register| {
            let properties = register_chunk(register, defaults);
            MemoryThingSingle::Register {
                properties,
                addr_offset: register.address_offset + peripheral_offset,
                register,
            }
        });

        let clusters = clusters.map(move |cluster| {
            let memory = cluster_chunks(cluster, defaults);
            MemoryThingSingle::Cluster {
                cluster,
                memory,
                addr_offset: cluster.address_offset + peripheral_offset,
            }
        });
        registers.chain(clusters)
    }

    pub fn len(&self) -> u64 {
        let Some(first) = self.chunks.first() else {
            return 0;
        };
        let last = self.chunks.last().unwrap();
        (last.offset() + last.len()) - first.offset()
    }

    pub fn finalize<'b>(
        self,
        context: &mut ContextMemoryGen<'a, 'b>,
    ) -> MemoryChunks<MemoryThingFinal> {
        let chunks = self
            .chunks
            .into_iter()
            .map(|chunk| chunk.finalize(context))
            .collect();
        MemoryChunks { chunks }
    }
}

impl<'a> MemoryChunk<MemoryThingCondensated<'a>> {
    fn len(&self) -> u64 {
        let Some(first) = self.things.first() else {
            return 0;
        };
        let last = self.things.last().unwrap();
        (last.offset() + last.len_total()) - first.offset()
    }
    fn offset(&self) -> u64 {
        self.things.first().map(|chunk| chunk.offset()).unwrap_or(0)
    }
    fn finalize<'b>(
        self,
        context: &mut ContextMemoryGen<'a, 'b>,
    ) -> MemoryChunk<MemoryThingFinal> {
        let things = self
            .things
            .into_iter()
            .map(|value| value.finalize(context))
            .collect();
        MemoryChunk { things }
    }
}

/// merge multiple chunks that are sequential, dsigned to be used only once
/// to avoid the complexity of merging chunks that are more then one
/// MemoryChunkType
fn condensate_chunks<'a>(
    mut things: Vec<MemoryThingSingle<'a>>,
) -> MemoryChunks<MemoryThingCondensated<'a>> {
    // sort chunks so lowest address comes first, also but biggest first to
    // optimize the algorithm below.
    things.sort_unstable_by(|a, b| {
        match a.page_offset().cmp(&b.page_offset()) {
            std::cmp::Ordering::Equal => b.len_total().cmp(&a.len_total()),
            x => x,
        }
    });

    let mut chunks: Vec<MemoryChunk<MemoryThingCondensated<'a>>> = vec![];
    // combine multiple chunks into pages
    for thing in things.into_iter() {
        match chunks.last_mut() {
            None => chunks.push(MemoryChunk {
                things: vec![thing.into()],
            }),
            Some(last) => {
                use core::cmp::Ordering::*;
                // merge the chunks if they are intersecting/sequential
                let last_end = last.offset() + last.len();
                match last_end.cmp(&thing.page_offset()) {
                    // the current chunk extend the last one, just concat it
                    Equal => last.things.push(thing.into()),
                    // overlapps with the last chunk
                    Greater => merge_overlapping_chunks(last, thing),
                    // no relation between those chunks
                    Less => chunks.push(MemoryChunk {
                        things: vec![thing.into()],
                    }),
                }
            }
        }
    }
    MemoryChunks { chunks }
}

fn find_offset_in_chunks(
    chunks: &[MemoryChunk<MemoryThingCondensated>],
    offset: u64,
) -> Option<(usize, usize, u64)> {
    let chunk_i = chunks.iter().position(|chunk| chunk.offset() <= offset)?;
    let chunk = &chunks[chunk_i];
    let (things_i, off) = find_offset_in_chunk(chunk, offset)?;
    Some((chunk_i, things_i, off))
}

fn find_offset_in_chunk(
    chunk: &MemoryChunk<MemoryThingCondensated>,
    offset: u64,
) -> Option<(usize, u64)> {
    for (thing_i, thing) in chunk.things.iter().enumerate() {
        // this offset is before this chunk, so no dice
        if thing.offset() > offset {
            return None;
        }
        // offset is in this chunk
        let thing_range = thing.offset()..thing.offset() + thing.len_total();
        if thing_range.contains(&offset) {
            return Some((thing_i, offset - thing.offset()));
        }
    }
    None
}

fn merge_overlapping_chunks<'a>(
    last: &mut MemoryChunk<MemoryThingCondensated<'a>>,
    thing: MemoryThingSingle<'a>,
) {
    // just need to check the last one, because they are sorted by offset
    let Some((things_i, off)) = find_offset_in_chunk(last, thing.page_offset())
    else {
        panic!("Invalid chunk overlapping")
    };
    let last_thing = &mut last.things[things_i];
    let same_start = off == 0;
    let same_len = last_thing.len_total() == thing.len_total();
    let last_greater = last_thing.len_total() > thing.len_total();
    use MemoryThingCondensated as Consdensated;
    use MemoryThingSingle as Single;
    match (last_thing, thing) {
        //TODO improve the merging of partially overlapping clusters and arrays
        // two identical register overlaps
        (
            Consdensated::Register { registers, .. },
            Single::Register { register, .. },
        ) if same_len && same_start => {
            //TODO read/write/properties are compatible
            registers.push(register)
        }
        // two registers overlap, the first contains the second due to dim
        (
            Consdensated::Register { registers, .. },
            Single::Register { register, .. },
        ) if last_greater && same_start => {
            //TODO read/write/properties are compatible
            registers.push(register)
        }
        // register overlap with other inside this cluster
        (
            Consdensated::Cluster {
                clusters,
                memory,
                addr_offset: address_offset,
                ..
            },
            Single::Register {
                register,
                properties,
                addr_offset: reg_offset,
            },
        ) if helper::dim_from_multiple(clusters).is_none() => {
            let offset_diff = reg_offset - *address_offset;
            merge_overlapping_register(
                memory,
                register,
                properties.size.unwrap() as u64,
                offset_diff as u64,
            );
        }
        // two cluster could be contain one another, in this case, just merge the
        // registers inside
        (
            Consdensated::Cluster {
                clusters, memory, ..
            },
            Single::Cluster {
                cluster,
                memory: memory2,
                ..
            },
        ) if (same_len || last_greater) && same_start => {
            //TODO read/write/properties are compatible
            merge_overlapping_clusters(memory, memory2);
            clusters.push(cluster);
        }
        (_last_thing, _thing) => {
            panic!(
                "Invalid chunk overlapping types at {:08x}",
                _thing.page_offset()
            )
        }
    }
}
fn merge_overlapping_clusters<'a>(
    mem1: &mut MemoryChunks<MemoryThingCondensated<'a>>,
    mem2: MemoryChunks<MemoryThingCondensated<'a>>,
) {
    if mem1.chunks.len() != mem2.chunks.len() {
        panic!("Merging cluster with diferent chunks")
    }
    // HACK: if mem2 is bigger then mem1 just try calling using the reverse
    // order LOL.
    if mem1.len() < mem2.len() {
        let mem1_copy: MemoryChunks<_> = mem1.clone();
        let mut mem2 = mem2;
        merge_overlapping_clusters(&mut mem2, mem1_copy);
        std::mem::swap(mem1, &mut mem2);
        return;
    }
    for (chunk1, chunk2) in mem1.chunks.iter_mut().zip(mem2.chunks) {
        if chunk1.offset() != chunk2.offset() {
            panic!("Merging cluster with diferent chunks offsets")
        }
        if chunk1.things.len() < chunk2.things.len() {
            panic!("Merging cluster with diferent chunks things")
        }
        for (thing1, thing2) in chunk1.things.iter_mut().zip(chunk2.things) {
            match (thing1, thing2) {
                (
                    MemoryThingCondensated::Register {
                        properties: properties1,
                        registers,
                        ..
                    },
                    MemoryThingCondensated::Register {
                        registers: registers2,
                        properties: properties2,
                        ..
                    },
                ) if properties1.size.unwrap() == properties2.size.unwrap() => {
                    registers.extend(registers2)
                }
                (
                    MemoryThingCondensated::Cluster { .. },
                    MemoryThingCondensated::Cluster { .. },
                ) => {
                    todo!("Implement recursive cluster merging")
                }
                _ => panic!("Merging cluster with diferent things"),
            }
        }
    }
}

fn merge_overlapping_register<'a>(
    mem: &mut MemoryChunks<MemoryThingCondensated<'a>>,
    register: &'a MaybeArray<RegisterInfo>,
    register_bytes: u64,
    offset: u64,
) {
    let Some((chunk_i, thing_i, chunk_offset)) =
        find_offset_in_chunks(&mem.chunks, offset)
    else {
        unreachable!()
    };
    let chunk = &mut mem.chunks[chunk_i];
    match &mut chunk.things[thing_i] {
        MemoryThingCondensated::Register {
            properties,
            addr_offset: _,
            registers,
        } => {
            if properties.size.unwrap() as u64 != register_bytes {
                panic!("invalid register inside a cluster overlap");
            }
            if chunk_offset != 0 {
                panic!("Invalid cluster with register overlapping")
            }
            registers.push(register);
        }
        MemoryThingCondensated::Cluster { memory, .. } => {
            merge_overlapping_register(
                memory,
                register,
                register_bytes,
                chunk_offset,
            );
        }
    }
}

fn register_chunk<'a>(
    register: &'a MaybeArray<RegisterInfo>,
    defaults: &RegisterProperties,
) -> RegisterProperties {
    let mut properties = *defaults;
    // the validation that can result into error is for the reset values,
    // that we don't care here
    properties
        .modify_from(register.properties, svd_parser::ValidateLevel::Disabled)
        .unwrap();
    let Some(bits) = properties.size else {
        panic!("Register don't have a size");
    };
    assert!(bits % 8 == 0);
    let bytes = (bits as u64 + 7) / 8;
    if let Some((_, dim_increment)) = register.array() {
        // TODO allow disconnected register dim? Just return one chunk for
        // each register
        if dim_increment as u64 != bytes {
            panic!(
                "Register with disconnected dim {} {} != {}",
                register.name(),
                dim_increment,
                bytes,
            );
        }
    }
    properties
}

fn cluster_chunks<'a>(
    cluster: &'a MaybeArray<ClusterInfo>,
    defaults: &RegisterProperties,
) -> MemoryChunks<MemoryThingCondensated<'a>> {
    if cluster.derived_from.is_some() {
        todo!();
    }
    // combine the defaults of the cluster with the parent
    let mut properties = *defaults;
    // the validation that can result into error is for the reset values,
    // that we don't care here
    properties
        .modify_from(
            cluster.default_register_properties,
            svd_parser::ValidateLevel::Disabled,
        )
        .unwrap();

    let things = MemoryChunks::new_chunk(
        &properties,
        0,
        cluster.registers(),
        cluster.clusters(),
    )
    .collect();
    let mem = condensate_chunks(things);

    let len = mem.len();
    if let Some((_dim, dim_increment)) = cluster.array() {
        // TODO allow cluster to have empty spaces at the end?
        // if so the len is just the dim_increment
        if (dim_increment as u64) < len {
            panic!(
                "Cluster {} {len} bytes, with invalid dim_len {dim_increment}",
                cluster.name()
            );
        }
    }

    mem
}

impl ContextMemoryGen<'_, '_> {
    fn clusters_name(&self) -> impl Iterator<Item = &str> {
        self.clusters.iter().map(|clusters| clusters[0].name())
    }
    fn clusters(&self) -> String {
        self.clusters_name()
            .map(|name| dim_to_n(&name.to_lowercase()))
            .zip(std::iter::repeat('_'.to_string()))
            .flat_map(|(c, s)| [s, c].into_iter())
            .collect()
    }
    pub fn gen_register_fun_name(&self, name: &str) -> String {
        format!(
            "{}{}_{}",
            self.pheriperals_name.to_lowercase(),
            self.clusters(),
            name
        )
    }

    pub fn gen_field_fun_name(&self, register: &str, field: &str) -> String {
        format!(
            "{}{}_{}_{}",
            self.pheriperals_name.to_lowercase(),
            self.clusters(),
            register,
            field
        )
    }
    pub fn gen_location(&self) -> String {
        let per = self.pheriperals_name;
        let clusters = self.clusters_name().collect::<Vec<_>>().join(", ");
        format!("{per} {clusters}")
    }
}

impl MemoryChunks<MemoryThingFinal> {
    pub fn gen_chunks<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
        read: bool,
        gen_code: impl FnMut((Range<u64>, TokenStream)) -> TokenStream,
    ) -> TokenStream {
        self.chunks
            .iter()
            .map(|chunk| (chunk.range(), chunk.gen_chunks(context, read)))
            .map(gen_code)
            .collect()
    }
    pub fn gen_register_fun<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
    ) -> TokenStream {
        self.chunks
            .iter()
            .map(|chunk| chunk.gen_register_functions(context))
            .collect()
    }
    pub fn gen_fields_fun<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
    ) -> TokenStream {
        self.chunks
            .iter()
            .map(|chunk| chunk.gen_fields_functions(context))
            .collect()
    }
}

impl MemoryChunk<MemoryThingFinal> {
    pub fn gen_chunks<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
        read: bool,
    ) -> TokenStream {
        // is somewhat common to have full blocks with only read/write
        // permissions
        let available_block = self.things.iter().any(|thing| {
            if read {
                thing.can_read()
            } else {
                thing.can_write()
            }
        });
        // if no available read/write for this chunk, don't bother
        if !available_block {
            let error = if read {
                quote! {ReadViolation}
            } else {
                quote! {WriteViolation}
            };
            quote! { return Err(MemError::#error); }
        } else {
            self.things
                .iter()
                .map(|thing| thing.gen_thing_mem_map(context, read))
                .collect()
        }
    }

    pub fn gen_register_functions<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
    ) -> TokenStream {
        self.things
            .iter()
            .map(|thing| thing.gen_register_functions(context))
            .collect()
    }

    fn gen_fields_functions<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
    ) -> TokenStream {
        self.things
            .iter()
            .map(|thing| thing.gen_fields_functions(context))
            .collect()
    }
}

impl MemoryThingFinal {
    fn gen_thing_mem_map<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
        read: bool,
    ) -> TokenStream {
        match self {
            Self::Cluster(cluster) => {
                context.clusters.push(cluster);
                let result =
                    self.gen_thing_mem_map_cluster(context, read, cluster);
                context.clusters.pop();
                result
            }
            Self::Register(reg) => {
                self.gen_thing_mem_map_register(context, read, reg)
            }
        }
    }

    fn gen_thing_mem_map_cluster<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
        read: bool,
        cluster: &'a ClusterAccess,
    ) -> TokenStream {
        //TODO read/write only if the chunk do something?
        // if not an array, just return a the simple mapping for the cluster
        // inner mappings
        let mem = if let Some((dim, dim_increment)) = cluster.array() {
            self.gen_thing_mem_map_cluster_array(
                context,
                read,
                cluster,
                &cluster.array_num_var,
                dim,
                dim_increment,
            )
        } else {
            // TODO this don't returns a error if a unmapped part of the cluster
            // is read/write
            cluster
                .memory
                .gen_chunks(context, read, |(_range, mem)| mem)
        };
        let chunk_start = cluster.offset();
        let chunk_end = chunk_start + cluster.len_total();
        let chunk_start_lit = Literal::u64_unsuffixed(chunk_start);
        let chunk_end_lit = Literal::u64_unsuffixed(chunk_end);
        let buf_ref = if read {
            quote! {&mut _buf}
        } else {
            quote! {&_buf}
        };
        let bytes = Literal::u64_unsuffixed(self.len_total());
        quote! {
            if _start < #chunk_end_lit && _end > #chunk_start_lit {
                // calculate the offset from the read to read from this chunk
                let _offset_start = #chunk_start.saturating_sub(_start);
                let _offset_end = _buf.len() as u64 - _end.saturating_sub(#chunk_end_lit);
                let _buf = #buf_ref[_offset_start as usize.._offset_end as usize];
                let _start = _start.saturating_sub(#chunk_start_lit);
                let _end = (_start + _buf.len() as u64).max(#bytes);
                #mem
            }
        }
    }

    fn gen_thing_mem_map_cluster_array<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
        read: bool,
        cluster: &'a ClusterAccess,
        array_name: &Ident,
        dim: u32,
        dim_increment: u32,
    ) -> TokenStream {
        let dim_lit = Literal::usize_suffixed(dim as usize);
        let len_lit = Literal::u32_unsuffixed(dim_increment);
        let gen_cond = |(range, mem): (Range<u64>, _)| {
            let start = Literal::u64_unsuffixed(range.start);
            let end = Literal::u64_unsuffixed(range.end);
            quote! { if _start < #end && _end > #start { #mem } }
        };
        let chunks = cluster.memory.gen_chunks(context, read, gen_cond);
        let buf_ref = if read {
            quote! {&mut _buf}
        } else {
            quote! {&_buf}
        };
        let bytes = Literal::u64_unsuffixed(self.len_element());
        quote! {
            // check for each cluster instance
            for #array_name in 0..#dim_lit {
                // calculate the start end and buffer to read
                // for this chunk
                // TODO separate this into a function?
                let _dim_offset = #array_name as u64 * #len_lit;
                // TODO make the calculation to find the index to start/end
                // don't use the if condition inside the loop to check if the
                // read/write is in range
                let _cluster_start = _dim_offset;
                let _cluster_end = _cluster_start + #len_lit;
                if _start < _cluster_end && _end > _cluster_start {
                    let _offset_start = _cluster_start.saturating_sub(_start);
                    let _offset_end = _buf.len() as u64 - _end.saturating_sub(_cluster_end);
                    let _buf = #buf_ref[_offset_start as usize.._offset_end as usize];
                    let _start = _start.saturating_sub(_cluster_start);
                    let _end = (_start + _buf.len() as u64).max(#bytes);
                    #chunks
                }
            }
        }
    }

    fn gen_thing_mem_map_register<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
        read: bool,
        reg: &'a RegisterAccess,
    ) -> TokenStream {
        let offset_start = self.offset();
        let offset_start_lit = Literal::u64_unsuffixed(offset_start);
        let offset_end_lit =
            Literal::u64_unsuffixed(self.offset() + self.len_total());
        if (read && reg.read_fun.is_none())
            || (!read && reg.write_fun.is_none())
        {
            let error = if read {
                quote! {ReadViolation}
            } else {
                quote! {WriteViolation}
            };
            return quote! {
                if _start < #offset_end_lit && _end > #offset_start_lit {
                    return Err(MemError::#error);
                }
            };
        }
        let call = if let Some((dim, dim_increment)) = self.array() {
            self.gen_thing_mem_map_register_array(
                context,
                read,
                reg,
                dim,
                dim_increment,
            )
        } else {
            self.gen_thing_mem_map_register_call(context, read, reg)
        };
        let reg_start = reg.address_offset as u64;
        let reg_start_lit = Literal::u64_unsuffixed(reg_start);
        let buf_ref = if read {
            quote! {&mut _buf}
        } else {
            quote! {&_buf}
        };
        let bytes = Literal::u64_unsuffixed(self.len_element());
        quote! {
            if _start < #offset_end_lit && _end > #offset_start_lit {
                let _offset_start = #offset_start.saturating_sub(_start);
                let _offset_end = _buf.len() as u64 - _end.saturating_sub(#offset_end_lit);
                let _buf = #buf_ref[_offset_start as usize.._offset_end as usize];
                let _start = _start.saturating_sub(#reg_start_lit);
                let _end = (_start + _buf.len() as u64).max(#bytes);
                #call
            }
        }
    }

    fn gen_thing_mem_map_register_array<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
        read: bool,
        reg: &'a RegisterAccess,
        dim: u32,
        dim_increment: u32,
    ) -> TokenStream {
        let dim_lit = Literal::usize_suffixed(dim as usize);
        let len_lit = Literal::u32_unsuffixed(dim_increment);
        let buf_ref = if read {
            quote! {&mut _buf}
        } else {
            quote! {&_buf}
        };
        let call = self.gen_thing_mem_map_register_call(context, read, reg);
        quote! {
            // check for each cluster instance
            for _dim in 0..#dim_lit {
                // calculate the start end and buffer to read
                // for this chunk
                // TODO separate this into a function?
                let _dim_offset = _dim as u64 * #len_lit;
                // TODO make the calculation to find the index to start/end
                // don't use the if condition inside the loop to check if the
                // read/write is in range
                let _reg_start = _dim_offset;
                let _reg_end = _reg_start + #len_lit;
                if _start < _reg_end && _end > _reg_start {
                    let _offset_start = _reg_start.saturating_sub(_start);
                    let _offset_end = _buf.len() as u64 - _end.saturating_sub(_reg_end);
                    let _buf = #buf_ref[_offset_start as usize.._offset_end as usize];
                    let _start = _start.saturating_sub(_reg_start);
                    let _end = (_end - _reg_end).min(#len_lit);
                    let _end = (_start + _buf.len() as u64).max(#len_lit);
                    #call
                }
            }
        }
    }

    fn gen_thing_mem_map_register_call<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
        read: bool,
        reg: &'a RegisterAccess,
    ) -> TokenStream {
        let context_use = context
            .clusters
            .iter()
            .filter(|clu| clu.array().is_some())
            .map(|cluster| {
                let dim_name = &cluster.array_num_var;
                quote! {#dim_name as usize}
            });
        let peripheral_use = (context.peripheral.instances.len() > 1)
            .then(|| quote! {_instance_page});
        let reg_use = self.array().map(|_array| quote! { _dim });
        let array_value = peripheral_use
            .into_iter()
            .chain(context_use)
            .chain(reg_use.into_iter());
        if read {
            let read = reg.read_fun.as_ref().unwrap();
            // TODO read using the buffer, like in writing, so uncessary
            // fields are not read
            // TODO implement byte endian here
            quote! {
                let value = self
                    .0
                    .lock()
                    .unwrap()
                    .#read(
                        #(#array_value,)*
                    )?
                    .to_ne_bytes();
                for _byte in _start.._end {
                    _buf[_byte as usize] = value[_byte as usize];
                }
            }
        } else {
            let write = reg.write_fun.as_ref().unwrap();
            quote! {
                self.0.lock().unwrap().#write(
                    #(#array_value,)*
                    _start,
                    _buf,
                )?;
            }
        }
    }

    fn gen_register_functions<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
    ) -> TokenStream {
        match self {
            Self::Register(reg) => {
                let mut tokens = TokenStream::new();
                context.register = Some(reg);
                reg.gen_register_function(context, &mut tokens);
                context.register = None;
                tokens
            }
            Self::Cluster(clu) => {
                context.clusters.push(clu);
                let tokens = clu.gen_register_functions(context);
                context.clusters.pop();
                tokens
            }
        }
    }

    fn gen_fields_functions<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
    ) -> TokenStream {
        match self {
            Self::Register(reg) => {
                let mut tokens = TokenStream::new();
                context.register = Some(reg);
                reg.gen_fields_functions(context, &mut tokens);
                context.register = None;
                tokens
            }
            Self::Cluster(clu) => {
                context.clusters.push(clu);
                let stream = clu.gen_fields_functions(context);
                context.clusters.pop();
                stream
            }
        }
    }
}
