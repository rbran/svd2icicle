use std::ops::Range;

use proc_macro2::{Literal, TokenStream};
use quote::quote;
use svd_parser::svd::{
    self, ClusterInfo, DimElement, MaybeArray, Name, RegisterInfo,
    RegisterProperties,
};

use crate::enumeration::EnumerationValues;
use crate::formater::dim_to_n;
use crate::helper::Dim;
use crate::peripheral::ContextCodeGen;
use crate::register::{ClusterAccess, RegisterAccess};
use crate::PAGE_LEN;

pub trait Memory {
    /// memory len in bytes
    fn len(&self) -> u64;
    /// start of the memory, in bytes, in relation what contains this memory
    fn offset(&self) -> u64;
    fn can_read(&self) -> bool;
    fn can_write(&self) -> bool;
    /// start and end of the memory in bytes
    fn range(&self) -> Range<u64> {
        self.offset()..self.offset() + self.len()
    }
}

pub trait MemoryThing: Memory + Dim {}

/// default implementation for Memory len for MemoryThing
fn memory_thing_len(mem: &impl MemoryThing, mut len: u64) -> u64 {
    if let Some(dim) = mem.array() {
        len = dim.dim_increment as u64 * dim.dim as u64;
    }
    len
}

#[derive(Debug)]
pub struct MemoryChunks<T> {
    chunks: Vec<MemoryChunk<T>>,
}

impl<T> Default for MemoryChunks<T> {
    fn default() -> Self {
        Self {
            chunks: Default::default(),
        }
    }
}

impl<T: Clone> Clone for MemoryChunks<T> {
    fn clone(&self) -> Self {
        Self {
            chunks: self.chunks.clone(),
        }
    }
}

#[derive(Debug)]
pub struct MemoryChunk<T> {
    things: Vec<T>,
}

impl<T> Default for MemoryChunk<T> {
    fn default() -> Self {
        Self {
            things: Default::default(),
        }
    }
}

impl<T: Clone> Clone for MemoryChunk<T> {
    fn clone(&self) -> Self {
        Self {
            things: self.things.clone(),
        }
    }
}

#[derive(Debug, Clone)]
enum MemoryThingSingle<'a> {
    Register {
        properties: RegisterProperties,
        register: &'a MaybeArray<RegisterInfo>,
    },
    Cluster {
        cluster: &'a MaybeArray<ClusterInfo>,
        memory: MemoryChunks<MemoryThingCondensated<'a>>,
    },
}

#[derive(Clone)]
pub(crate) enum MemoryThingCondensated<'a> {
    Register {
        properties: RegisterProperties,
        registers: Vec<&'a MaybeArray<RegisterInfo>>,
    },
    Cluster {
        clusters: Vec<&'a MaybeArray<ClusterInfo>>,
        memory: MemoryChunks<MemoryThingCondensated<'a>>,
    },
}

#[derive(Debug)]
pub enum MemoryThingFinal {
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

impl Memory for MemoryThingFinal {
    fn len(&self) -> u64 {
        let bytes = match self {
            Self::Register(reg) => reg.properties.size.unwrap() as u64 / 8,
            Self::Cluster(clu) => clu.memory.len(),
        };
        memory_thing_len(self, bytes)
    }

    fn offset(&self) -> u64 {
        match self {
            Self::Register(reg) => reg.address_offset as u64,
            Self::Cluster(clu) => clu.address_offset as u64,
        }
    }

    fn can_read(&self) -> bool {
        match self {
            Self::Register(reg) => {
                reg.properties.access.unwrap_or_default().can_read()
            }
            Self::Cluster(clu) => clu.memory.can_read(),
        }
    }

    fn can_write(&self) -> bool {
        match self {
            Self::Register(reg) => {
                reg.properties.access.unwrap_or_default().can_write()
            }
            Self::Cluster(clu) => clu.memory.can_write(),
        }
    }
}

impl Dim for MemoryThingFinal {
    fn array(&self) -> Option<&DimElement> {
        match self {
            Self::Register(reg) => reg.array.as_ref(),
            Self::Cluster(clu) => clu.dim.as_ref().map(|x| &x.1),
        }
    }
}

impl MemoryThing for MemoryThingFinal {}

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

impl<T: Memory> Memory for MemoryChunks<T> {
    fn len(&self) -> u64 {
        self.chunks.iter().map(|thing| thing.len()).sum()
    }
    fn offset(&self) -> u64 {
        0
    }
    fn can_read(&self) -> bool {
        self.chunks
            .iter()
            .map(|chunk| chunk.can_read())
            .reduce(|a, b| a | b)
            .unwrap_or(true)
    }
    fn can_write(&self) -> bool {
        self.chunks
            .iter()
            .map(|chunk| chunk.can_write())
            .reduce(|a, b| a | b)
            .unwrap_or(true)
    }
}

impl<T: Memory> Memory for MemoryChunk<T> {
    fn len(&self) -> u64 {
        self.things.iter().map(|thing| thing.len()).sum()
    }
    fn offset(&self) -> u64 {
        self.things[0].offset()
    }
    fn can_read(&self) -> bool {
        self.things
            .iter()
            .map(|things| things.can_read())
            .reduce(|a, b| a | b)
            .unwrap_or(true)
    }
    fn can_write(&self) -> bool {
        self.things
            .iter()
            .map(|things| things.can_write())
            .reduce(|a, b| a | b)
            .unwrap_or(true)
    }
}

impl Dim for MemoryThingSingle<'_> {
    fn array(&self) -> Option<&DimElement> {
        match self {
            Self::Register { register, .. } => register.array(),
            Self::Cluster { cluster, .. } => cluster.array(),
        }
    }
}
impl MemoryThing for MemoryThingSingle<'_> {}

impl Memory for MemoryThingSingle<'_> {
    fn len(&self) -> u64 {
        let bytes = match self {
            Self::Register { properties, .. } => {
                properties.size.unwrap() as u64 / 8
            }
            Self::Cluster { memory, .. } => memory.len(),
        };
        memory_thing_len(self, bytes)
    }
    fn offset(&self) -> u64 {
        match self {
            Self::Register { register, .. } => register.address_offset as u64,
            Self::Cluster { cluster, .. } => cluster.address_offset as u64,
        }
    }

    fn can_read(&self) -> bool {
        unreachable!()
    }

    fn can_write(&self) -> bool {
        unreachable!()
    }
}

impl Dim for MemoryThingCondensated<'_> {
    fn array(&self) -> Option<&DimElement> {
        match self {
            Self::Register { registers, .. } => registers[0].array(),
            Self::Cluster { clusters, .. } => clusters[0].array(),
        }
    }
}
impl MemoryThing for MemoryThingCondensated<'_> {}

impl Memory for MemoryThingCondensated<'_> {
    fn len(&self) -> u64 {
        let bytes = match self {
            Self::Register { properties, .. } => {
                properties.size.unwrap() as u64 / 8
            }
            Self::Cluster { memory, .. } => memory.len(),
        };
        memory_thing_len(self, bytes)
    }
    fn offset(&self) -> u64 {
        match self {
            Self::Register { registers, .. } => {
                registers[0].address_offset as u64
            }
            Self::Cluster { clusters, .. } => clusters[0].address_offset as u64,
        }
    }
    fn range(&self) -> Range<u64> {
        self.offset()..self.offset() + self.len()
    }

    fn can_read(&self) -> bool {
        unreachable!()
    }

    fn can_write(&self) -> bool {
        unreachable!()
    }
}

impl<'a> From<MemoryThingSingle<'a>> for MemoryThingCondensated<'a> {
    fn from(value: MemoryThingSingle<'a>) -> Self {
        match value {
            MemoryThingSingle::Register {
                properties,
                register,
            } => Self::Register {
                properties,
                registers: vec![register],
            },
            MemoryThingSingle::Cluster { cluster, memory } => Self::Cluster {
                clusters: vec![cluster],
                memory,
            },
        }
    }
}

impl MemoryChunks<MemoryThingFinal> {
    pub(crate) fn new_page<'a>(
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
                MemoryChunks::new_chunk(
                    defaults,
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

    pub(crate) fn gen_fields_functions<'a>(
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
        registers: impl Iterator<Item = &'a MaybeArray<RegisterInfo>> + 'b,
        clusters: impl Iterator<Item = &'a MaybeArray<ClusterInfo>> + 'b,
    ) -> impl Iterator<Item = MemoryThingSingle<'a>> + 'b {
        let registers = registers.map(|register| {
            let properties = register_chunk(register, defaults);
            MemoryThingSingle::Register {
                properties,
                register,
            }
        });

        let clusters = clusters.map(|cluster| {
            let memory = cluster_chunks(cluster, defaults);
            MemoryThingSingle::Cluster { cluster, memory }
        });
        registers.chain(clusters)
    }
}

impl<'a> MemoryChunks<MemoryThingCondensated<'a>> {
    pub(crate) fn finalize<'b>(
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

impl<'a> MemoryThingCondensated<'a> {
    fn finalize<'b>(
        self,
        context: &mut ContextMemoryGen<'a, 'b>,
    ) -> MemoryThingFinal {
        match self {
            Self::Register {
                properties,
                registers,
            } => MemoryThingFinal::Register(RegisterAccess::new(
                context, properties, registers,
            )),
            Self::Cluster { clusters, memory } => MemoryThingFinal::Cluster(
                ClusterAccess::new(context, clusters, memory),
            ),
        }
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
        let a = a.range();
        let b = b.range();
        match a.start.cmp(&b.start) {
            std::cmp::Ordering::Equal => b.end.cmp(&a.end),
            x => x,
        }
    });

    let mut chunks = vec![];
    // combine multiple chunks into pages
    for thing in things.into_iter() {
        match chunks.last_mut() {
            None => chunks.push(MemoryChunk {
                things: vec![thing.into()],
            }),
            Some(last) => {
                use core::cmp::Ordering::*;
                // merge the chunks if they are intersecting/sequential
                let last_end = last.range().end;
                match last_end.cmp(&thing.offset()) {
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

fn find_offset_in_chunks<T: Memory>(
    chunks: &[MemoryChunk<T>],
    offset: u64,
) -> Option<(usize, usize, u64)> {
    let chunk_i = chunks.iter().position(|chunk| chunk.offset() <= offset)?;
    let chunk = &chunks[chunk_i];
    let (things_i, off) = find_offset_in_chunk(chunk, offset)?;
    Some((chunk_i, things_i, off))
}

fn find_offset_in_chunk<T: Memory>(
    chunk: &MemoryChunk<T>,
    offset: u64,
) -> Option<(usize, u64)> {
    for (thing_i, thing) in chunk.things.iter().enumerate() {
        // this offset is before this chunk, so no dice
        if thing.offset() > offset {
            return None;
        }
        // offset is in this chunk
        if thing.range().contains(&offset) {
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
    let Some((things_i, off)) = find_offset_in_chunk(last, thing.offset()) else {
        panic!("Invalid chunk overlapping")
    };
    let last_thing = &mut last.things[things_i];
    let same_start = off == 0;
    let same_len = last_thing.len() == thing.len();
    let last_greater = last_thing.len() > thing.len();
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
                clusters, memory, ..
            },
            Single::Register {
                register,
                properties,
            },
        ) if clusters[0].is_one() => {
            let offset_diff = register.address_offset as u64
                - clusters[0].address_offset as u64;
            merge_overlapping_register(
                memory,
                register,
                properties.size.unwrap() as u64,
                offset_diff,
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
            panic!("Invalid chunk overlapping types at {:08x}", _thing.offset())
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
    let Some((chunk_i, thing_i, chunk_offset)) = find_offset_in_chunks(&mem.chunks, offset) else {
        unreachable!()
    };
    let chunk = &mut mem.chunks[chunk_i];
    match &mut chunk.things[thing_i] {
        MemoryThingCondensated::Register {
            properties,
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
    if let Some(dim) = register.array() {
        // TODO allow disconnected register dim? Just return one chunk for
        // each register
        if dim.dim_increment as u64 != bytes {
            panic!(
                "Register with disconnected dim {} {} != {}",
                register.name(),
                dim.dim_increment,
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
        cluster.registers(),
        cluster.clusters(),
    )
    .collect();
    let mem = condensate_chunks(things);

    let len = mem.len();
    if let Some(dim) = cluster.array() {
        // TODO allow cluster to have empty spaces at the end?
        // if so the len is just the dim_increment
        if (dim.dim_increment as u64) < len {
            panic!(
                "Cluster {} {len} bytes, with invalid dim {dim:#?}",
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
    pub(crate) fn gen_match_chunks<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
        read: bool,
        offset: u64,
    ) -> TokenStream {
        self.chunks
            .iter()
            .map(|chunk| chunk.gen_match_chunks(context, read, offset))
            .collect()
    }
    pub(crate) fn gen_chunks<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
        read: bool,
        offset: u64,
    ) -> TokenStream {
        self.chunks
            .iter()
            .map(|chunk| chunk.gen_chunks(context, read, offset))
            .collect()
    }
    pub(crate) fn gen_register_fun<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
    ) -> TokenStream {
        self.chunks
            .iter()
            .map(|chunk| chunk.gen_register_functions(context))
            .collect()
    }
    pub(crate) fn gen_fields_fun<'a>(
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
    pub(crate) fn gen_match_chunks<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
        read: bool,
        offset: u64,
    ) -> TokenStream {
        let range = self.range();
        let start = (range.start != 0)
            .then_some(Literal::u64_unsuffixed(range.start + offset));
        let end = Literal::u64_unsuffixed(range.end + offset);
        let startp1 = Literal::u64_unsuffixed(range.start + 1 + offset);
        let endm1 = Literal::u64_unsuffixed((range.end - 1) + offset);
        //is somewhat common to have full blocks with only read/write
        //permissions
        let call = self.gen_chunks(context, read, offset);
        quote! {
            (#start..=#endm1, #startp1..=#end) => {
                #call
            },
        }
    }
    pub(crate) fn gen_chunks<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
        read: bool,
        offset: u64,
    ) -> TokenStream {
        //is somewhat common to have full blocks with only read/write
        //permissions
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
                .map(|thing| thing.gen_thing_mem_map(context, read, offset))
                .collect()
        }
    }

    pub(crate) fn gen_register_functions<'a>(
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
        offset: u64,
    ) -> TokenStream {
        let range = self.range();
        let offset_start = range.start + offset;
        let offset_end = range.end + offset;
        let offset_start_lit = Literal::u64_unsuffixed(offset_start);
        let offset_end_lit = Literal::u64_unsuffixed(offset_end);
        let array_num = self.array().map(|array| {
            let len_lit = Literal::u32_unsuffixed(array.dim_increment);
            quote! {
                let _dim = ((_start - #offset_start_lit) % #len_lit) as usize;
            }
        });
        let peripheral_use = (context.peripheral.instances.len() > 1)
            .then(|| quote! {_instance_page as usize});
        let context_use = context
            .clusters
            .iter()
            .filter_map(|clu| clu.dim.as_ref())
            .map(|(dim_name, _dim)| quote! {#dim_name});
        let reg_use = self.array().map(|_array| quote! { _dim });
        let array_value = peripheral_use
            .into_iter()
            .chain(context_use)
            .chain(reg_use.into_iter());
        match (read, self) {
            (_, Self::Cluster(cluster)) => {
                //TODO read/write only chunk do something?
                if let Some((array_name, array)) = cluster.dim.as_ref() {
                    context.clusters.push(cluster);
                    let mem = cluster.memory.gen_match_chunks(
                        context,
                        read,
                        offset + self.offset(),
                    );
                    context.clusters.pop();
                    let len_lit = Literal::u32_unsuffixed(array.dim_increment);
                    //TODO read between clusters
                    quote! {
                        let #array_name = ((_start - #offset_start_lit) % #len_lit) as usize;
                        match (_start, _end) {
                            #mem
                            _ => return Err(MemError::Unmapped),
                        }
                    }
                } else {
                    cluster.memory.gen_chunks(
                        context,
                        read,
                        offset + self.offset(),
                    )
                }
            }
            (true, Self::Register(reg)) => {
                //TODO check for multiple dim from the the cluster
                let call = if let Some(read) = reg.read_fun.as_ref() {
                    let bytes = (offset_start..offset_end).enumerate().map(
                        |(byte_i, byte)| {
                            let byte = Literal::u64_unsuffixed(byte);
                            let byte_i = Literal::usize_unsuffixed(byte_i);
                            quote! {
                                if _start <= #byte && _end > #byte {
                                    _buf[(#byte - _start) as usize] =
                                        value[#byte_i];
                                }
                            }
                        },
                    );
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
                        #(#bytes)*
                    }
                } else {
                    quote! { return Err(MemError::ReadViolation); }
                };
                quote! {
                    if _start < #offset_end_lit && _end > #offset_start_lit {
                        #array_num
                        #call
                    }
                }
            }
            (false, Self::Register(reg)) => {
                if let Some(write) = reg.write_fun.as_ref() {
                    quote! {
                        if _start < #offset_end_lit && _end > #offset_start_lit {
                            #array_num
                            let offset = _start.saturating_sub(#offset_start_lit);
                            let start = #offset_start.saturating_sub(_start) as usize;
                            let end = ((_end - #offset_start_lit) - offset) as usize;
                            self.0.lock().unwrap().#write(
                                #(#array_value,)*
                                offset,
                                &_buf[start..end],
                            )?;
                        }
                    }
                } else {
                    quote! {
                        if _start < #offset_end_lit && _end > #offset_start_lit {
                            return Err(MemError::WriteViolation);
                        }
                    }
                }
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
