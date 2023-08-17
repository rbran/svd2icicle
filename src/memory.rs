use std::ops::Range;

use proc_macro2::{Literal, TokenStream};
use quote::quote;
use svd_parser::svd::{
    ClusterInfo, Device, DimElement, MaybeArray, Name, PeripheralInfo,
    RegisterInfo, RegisterProperties,
};

use crate::helper::Dim;
use crate::peripheral::PeripheralPage;
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

pub trait MemoryThing: Memory {
    //fn properties(&self) -> &RegisterProperties;
    fn array(&self) -> Option<&DimElement>;
}

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

impl<T> Clone for MemoryChunks<T>
where
    T: Clone,
{
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

impl<T> Clone for MemoryChunk<T>
where
    T: Clone,
{
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

pub enum MemoryThingFinal<'a> {
    Register(RegisterAccess<'a>),
    Cluster(ClusterAccess<'a>),
}

#[derive(Debug, Clone)]
pub(crate) struct Context<'a> {
    //pheriperals: &'b [&'a MaybeArray<PeripheralInfo>],
    pub pheriperals_name: &'a str,
    pub clusters: Vec<String>,
}

impl Memory for MemoryThingFinal<'_> {
    fn len(&self) -> u64 {
        let bytes = match self {
            Self::Register(reg) => reg.properties.size.unwrap() as u64 / 8,
            Self::Cluster(clu) => clu.memory.len(),
        };
        memory_thing_len(self, bytes)
    }

    fn offset(&self) -> u64 {
        match self {
            Self::Register(reg) => reg.registers[0].address_offset as u64,
            Self::Cluster(clu) => clu.clusters[0].address_offset as u64,
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

impl MemoryThing for MemoryThingFinal<'_> {
    fn array(&self) -> Option<&DimElement> {
        match self {
            Self::Register(reg) => reg.registers[0].array(),
            Self::Cluster(clu) => clu.clusters[0].array(),
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

impl MemoryThing for MemoryThingSingle<'_> {
    fn array(&self) -> Option<&DimElement> {
        match self {
            Self::Register { register, .. } => register.array(),
            Self::Cluster { cluster, .. } => cluster.array(),
        }
    }
}

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

impl MemoryThing for MemoryThingCondensated<'_> {
    fn array(&self) -> Option<&DimElement> {
        match self {
            Self::Register { registers, .. } => registers[0].array(),
            Self::Cluster { clusters, .. } => clusters[0].array(),
        }
    }
}

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

impl<'a> MemoryChunks<MemoryThingFinal<'a>> {
    pub fn new_page(
        device: &'a Device,
        name: &str,
        peripherals: &[&'a MaybeArray<PeripheralInfo>],
    ) -> Self {
        let things = peripherals
            .iter()
            .flat_map(|peripheral| {
                let mut peripheral = *peripheral;
                if let Some(derived) = &peripheral.derived_from {
                    peripheral = device.get_peripheral(derived).unwrap();
                }
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
        let mut context = Context {
            //pheriperals,
            pheriperals_name: name,
            clusters: vec![],
        };
        let chunks = chunks.finalize(&mut context, device);
        chunks
    }

    pub(crate) fn gen_fields_functions(
        &self,
        peripheral: &PeripheralPage,
    ) -> TokenStream {
        self.chunks
            .iter()
            .map(|chunk| chunk.gen_fields_functions(peripheral))
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
        context: &mut Context<'b>,
        device: &'a Device,
    ) -> MemoryChunks<MemoryThingFinal<'a>> {
        let chunks = self
            .chunks
            .into_iter()
            .map(|chunk| chunk.finalize(context, device))
            .collect();
        MemoryChunks { chunks }
    }
}

impl<'a> MemoryChunk<MemoryThingCondensated<'a>> {
    fn finalize<'b>(
        self,
        context: &mut Context<'b>,
        device: &'a Device,
    ) -> MemoryChunk<MemoryThingFinal<'a>> {
        let things = self
            .things
            .into_iter()
            .map(|value| value.finalize(context, device))
            .collect();
        MemoryChunk { things }
    }
}

impl<'a> MemoryThingCondensated<'a> {
    fn finalize<'b>(
        self,
        context: &mut Context<'b>,
        device: &'a Device,
    ) -> MemoryThingFinal<'a> {
        match self {
            Self::Register {
                properties,
                registers,
            } => MemoryThingFinal::Register(RegisterAccess::new(
                context, device, properties, registers,
            )),
            Self::Cluster { clusters, memory } => MemoryThingFinal::Cluster(
                ClusterAccess::new(context, device, clusters, memory),
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

impl Context<'_> {
    pub fn gen_register_fun_name(&self, name: &str) -> String {
        let mut final_name = self.pheriperals_name.to_lowercase();
        let clusters = self.clusters.iter().map(String::as_str);
        let name = [name];
        for name in clusters.chain(name) {
            final_name.push('_');
            final_name.push_str(name);
        }
        final_name
    }

    pub fn gen_field_fun_name(&self, register: &str, field: &str) -> String {
        let mut final_name = self.pheriperals_name.to_lowercase();
        let clusters = self.clusters.iter().map(String::as_str);
        let name = [register, field];
        for name in clusters.chain(name) {
            final_name.push('_');
            final_name.push_str(name);
        }
        final_name
    }
}

impl MemoryChunks<MemoryThingFinal<'_>> {
    pub fn gen_match_chunks(&self, read: bool, offset: u64) -> TokenStream {
        self.chunks
            .iter()
            .map(|chunk| chunk.gen_match_chunks(read, offset))
            .collect()
    }
    pub fn gen_chunks(&self, read: bool, offset: u64) -> TokenStream {
        self.chunks
            .iter()
            .map(|chunk| chunk.gen_chunks(read, offset))
            .collect()
    }
    pub fn gen_register_fun(&self, peripheral: &PeripheralPage) -> TokenStream {
        self.chunks
            .iter()
            .map(|chunk| chunk.gen_register_functions(peripheral))
            .collect()
    }
    pub fn gen_fields_fun(&self, peripheral: &PeripheralPage) -> TokenStream {
        self.chunks
            .iter()
            .map(|chunk| chunk.gen_fields_functions(peripheral))
            .collect()
    }
}

impl MemoryChunk<MemoryThingFinal<'_>> {
    pub fn gen_match_chunks(&self, read: bool, offset: u64) -> TokenStream {
        let range = self.range();
        let start = (range.start != 0)
            .then_some(Literal::u64_unsuffixed(range.start + offset));
        let end = Literal::u64_unsuffixed(range.end + offset);
        let startp1 = Literal::u64_unsuffixed(range.start + 1 + offset);
        let endm1 = Literal::u64_unsuffixed((range.end - 1) + offset);
        //is somewhat common to have full blocks with only read/write
        //permissions
        let call = self.gen_chunks(read, offset);
        quote! {
            (#start..=#endm1, #startp1..=#end) => {
                #call
            },
        }
    }
    pub fn gen_chunks(&self, read: bool, offset: u64) -> TokenStream {
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
                .map(|thing| thing.gen_thing_mem_map(read, offset))
                .collect()
        }
    }

    pub fn gen_register_functions(
        &self,
        peripheral: &PeripheralPage,
    ) -> TokenStream {
        self.things
            .iter()
            .map(|thing| thing.gen_register_functions(peripheral))
            .collect()
    }

    fn gen_fields_functions(&self, peripheral: &PeripheralPage) -> TokenStream {
        self.things
            .iter()
            .map(|thing| thing.gen_fields_functions(peripheral))
            .collect()
    }
}

impl<'a> MemoryThingFinal<'a> {
    fn gen_thing_mem_map(&self, read: bool, offset: u64) -> TokenStream {
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
        let array_value = self.array().map(|_dim| quote! {_dim});
        match (read, self) {
            (_, Self::Cluster(cluster)) => {
                //TODO read/write only chunk do something?
                if cluster.clusters[0].array().is_some() {
                    let mem = cluster
                        .memory
                        .gen_match_chunks(read, offset + self.offset());
                    quote! {
                        #array_num
                        match (_start, _end) {
                            #mem
                            _ => return Err(MemError::Unmapped),
                        }
                    }
                } else {
                    cluster.memory.gen_chunks(read, offset + self.offset())
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
                            .#read(#array_value)?
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
                    let array_value = array_value.into_iter();
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

    fn gen_register_functions(
        &self,
        peripheral: &PeripheralPage,
    ) -> TokenStream {
        match self {
            Self::Register(reg) => {
                let mut tokens = TokenStream::new();
                reg.gen_register_function(peripheral, &mut tokens);
                tokens
            }
            Self::Cluster(clu) => clu.gen_register_functions(peripheral),
        }
    }

    fn gen_fields_functions(&self, peripheral: &PeripheralPage) -> TokenStream {
        match self {
            Self::Register(reg) => {
                let mut tokens = TokenStream::new();
                reg.gen_fields_functions(peripheral, &mut tokens);
                tokens
            }
            Self::Cluster(clu) => clu.gen_fields_functions(peripheral),
        }
    }
}
