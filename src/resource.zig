const std = @import("std");

const MMTypes = @import("MMTypes.zig");
const Stream = @import("stream.zig");

pub const MMStream = Stream.MMStream(std.io.FixedBufferStream([]const u8));

pub const Resource = struct {
    stream: MMStream,
    allocator: std.mem.Allocator,
    type: MMTypes.ResourceType,
    dependencies: ?[]const Dependency,

    pub fn deinit(self: Resource) void {
        self.allocator.free(self.stream.stream.buffer);
        if (self.dependencies) |dependencies|
            self.allocator.free(dependencies);
    }
};

pub fn writeResource(
    resource_type: MMTypes.ResourceType,
    compression_flags: MMTypes.CompressionFlags,
    dependencies: []const Dependency,
    revision: MMTypes.Revision,
    stream: *std.io.StreamSource,
    data: []const u8,
    allocator: std.mem.Allocator,
) !void {
    //Compressing files <1k is probably not worth it, lets leave it uncompressed
    const compress = data.len > 1024 and (revision.head >= 0x189 and resource_type != .static_mesh);

    const writer = stream.writer();

    //Write the header type
    _ = try writer.writeAll(&(MMTypes.resourceTypeToHeader(resource_type).?));

    //TODO: make this not hardcoded!
    const serialization_method: MMTypes.SerializationMethod = .binary;
    try writer.writeByte(@intFromEnum(serialization_method));

    //Write out the revision head
    try writer.writeInt(u32, revision.head, .big);

    const dependency_table_offset_pos = try stream.getPos();

    //In this revision the dependency table was added
    if (revision.head >= 0x109) {
        //Write a standin value, which will later be replaced by the dependency offset
        try writer.writeInt(u32, undefined, .big);
    }

    //In this revision, compression flag was added to non-static mesh assets
    if (revision.head >= 0x189) {
        if (resource_type == .static_mesh)
            @panic("Unimplemented, sorry");

        //In this revision, branch id and branch offset was branch revision was added
        if (revision.head >= 0x271) {
            try writer.writeInt(u16, revision.branch_id, .big);
            try writer.writeInt(u16, revision.branch_revision, .big);
        }

        //In revision 0x272 on the non-zero branch, compression flags were added. In revision 0x297, compression flags were made manditory
        if (revision.head >= 0x297 or (revision.head == 0x272 and revision.branch_id != 0)) {
            try writer.writeByte(@bitCast(compression_flags));
        }

        //Write whether its compressed with zlib compression
        try writer.writeByte(if (compress) 1 else 0);
    }

    if (serialization_method == .encrypted_binary)
        @panic("Not implemented, sorry");

    if (compress) {
        const chunk_size = 0x8000;

        var compress_arena = std.heap.ArenaAllocator.init(allocator);
        defer compress_arena.deinit();
        const compress_allocator = compress_arena.allocator();

        //Init with enough capacity to all but *assure* we have already allocated enough for every chunk beforehand.
        var sizes = try std.ArrayList(struct { usize, usize }).initCapacity(compress_allocator, data.len / chunk_size);
        var final_data = std.ArrayList(u8).init(compress_allocator);

        var data_stream = std.io.fixedBufferStream(data);
        const reader = data_stream.reader();

        var read = try reader.readBoundedBytes(chunk_size);
        while (read.len > 0) : (read = try reader.readBoundedBytes(chunk_size)) {
            //Since compress may make multiple read/write calls, to lower the allocation *count*,
            // lets ensure theres AT LEAST enough data here to fit the full uncompressed chunk, so theres likely only one allocation per iteration.
            try final_data.ensureUnusedCapacity(read.len);

            var uncompressed_data_stream = std.io.fixedBufferStream(read.constSlice());
            var counting_writer = std.io.countingWriter(final_data.writer());
            try std.compress.zlib.compress(uncompressed_data_stream.reader(), counting_writer.writer(), .{ .level = .best });

            try sizes.append(.{ read.len, @bitCast(counting_writer.bytes_written) });
        }

        //Write the unknown flags (always 0x0001 apparently)
        try writer.writeInt(u16, 0x0001, .big);
        //Write the amount of chunks
        try writer.writeInt(u16, @intCast(sizes.items.len), .big);

        //Write all the chunk sizes
        for (sizes.items) |size| {
            const uncompressed_size, const compressed_size = size;

            try writer.writeInt(u16, @intCast(compressed_size), .big);
            try writer.writeInt(u16, @intCast(uncompressed_size), .big);
        }

        //Write all the chunks to the output stream
        try writer.writeAll(final_data.items);
    } else {
        try writer.writeAll(data);
    }

    const dependency_table_offset = try stream.getPos();

    //Write out all the dependencies
    try writer.writeInt(u32, @intCast(dependencies.len), .big);
    for (dependencies) |dependency| {
        try writer.writeByte(@intFromEnum(dependency.ident));
        switch (dependency.ident) {
            .guid => |guid| {
                try writer.writeInt(u32, guid, .big);
            },
            .hash => |hash| {
                try writer.writeAll(&hash);
            },
        }
        try writer.writeInt(u32, @intFromEnum(dependency.type), .big);
    }

    //Seek to where we need to write the dependency table offset
    try stream.seekTo(dependency_table_offset_pos);
    //Write the dependency table offset
    try writer.writeInt(u32, @intCast(dependency_table_offset), .big);

    //Seek back to the end
    try stream.seekTo(try stream.getEndPos());
}

pub fn readResource(read_buf: []const u8, allocator: std.mem.Allocator) !Resource {
    var stream = std.io.fixedBufferStream(read_buf);
    const raw_reader = stream.reader();

    var magic: [3]u8 = undefined;
    _ = try raw_reader.readAll(&magic);

    const resource_type: MMTypes.ResourceType = MMTypes.headerToResourceType(magic) orelse return error.UnknownResourceType;

    const serialization_method = try std.meta.intToEnum(MMTypes.SerializationMethod, try raw_reader.readByte());

    switch (serialization_method) {
        .binary, .encrypted_binary => {
            var revision = MMTypes.Revision{
                .head = try raw_reader.readInt(u32, .big),
                .branch_id = 0,
                .branch_revision = 0,
            };
            var compression_flags: MMTypes.CompressionFlags = .{
                .compressed_integers = false,
                .compressed_matrices = false,
                .compressed_vectors = false,
            };
            var compressed = false;

            var dependency_table_offset: ?usize = null;
            var dependencies: ?[]const Dependency = null;

            if (revision.head >= 0x109) {
                dependency_table_offset, dependencies = try getDependencies(&stream, allocator);

                if (revision.head >= 0x189) {
                    if (resource_type == .static_mesh)
                        @panic("Unable to read static meshes yet sorry");

                    // NOTE(Aidan): Were they actually added on 0x27a, but how can it be on 0x272
                    // then?!
                    if (revision.head >= 0x271) {
                        revision.branch_id = try raw_reader.readInt(u16, .big);
                        revision.branch_revision = try raw_reader.readInt(u16, .big);
                    }

                    if (revision.head >= 0x297 or (revision.head == 0x272 and revision.branch_id != 0)) {
                        compression_flags = @bitCast(try raw_reader.readByte());
                    }

                    compressed = try raw_reader.readByte() != 0;
                }
            }

            if (serialization_method == .encrypted_binary)
                @panic("Encrypted binary decryption not implemented yet");

            var mm_stream = MMStream{
                .stream = stream,
                .compression_flags = compression_flags,
                .revision = revision,
            };

            const buffer = if (compressed) blk: {
                //Some kind of flags
                _ = try mm_stream.readInt(u16);

                const chunks = try mm_stream.readInt(u16);

                if (chunks == 0) {
                    //We unwrap dependency_table_offset since the offset is always filled out when compression is set
                    break :blk try allocator.dupe(u8, mm_stream.stream.buffer[stream.pos..dependency_table_offset.?]);
                }

                const bufs = try allocator.alloc(u16, chunks * 2);
                defer allocator.free(bufs);

                const compressed_sizes = bufs[0..chunks];
                const uncompressed_sizes = bufs[chunks..];

                var full_size: usize = 0;
                for (compressed_sizes, uncompressed_sizes) |*compressed_size, *uncompressed_size| {
                    compressed_size.* = try mm_stream.readInt(u16);
                    uncompressed_size.* = try mm_stream.readInt(u16);

                    full_size += uncompressed_size.*;
                }

                const decompressed_buf = try allocator.alloc(u8, full_size);
                errdefer allocator.free(decompressed_buf);

                var decompressed_writer = std.io.fixedBufferStream(decompressed_buf);

                // Iterate over all chunks, reading the compressed data, and decompressing it
                for (compressed_sizes) |compressed_size| {
                    var limited_reader = std.io.limitedReader(mm_stream.stream.reader(), compressed_size);

                    try std.compress.zlib.decompress(limited_reader.reader(), decompressed_writer.writer());
                }

                if (decompressed_writer.pos != decompressed_buf.len) {
                    return error.DecompressedTooFewBytes;
                }

                break :blk decompressed_buf;
            } else if (dependency_table_offset) |end_idx|
                try allocator.dupe(u8, mm_stream.stream.buffer[stream.pos..end_idx])
            else
                try allocator.dupe(u8, mm_stream.stream.buffer[stream.pos..]);
            errdefer allocator.free(buffer);

            mm_stream.stream = std.io.fixedBufferStream(@as([]const u8, buffer));

            return .{
                .stream = mm_stream,
                .allocator = allocator,
                .type = resource_type,
                .dependencies = dependencies,
            };
        },
        else => std.debug.panic("Unknown serialization method {s}\n", .{@tagName(serialization_method)}),
    }
}

const Dependency = struct {
    type: MMTypes.ResourceType,
    ident: MMTypes.ResourceIdentifier,
};

fn getDependencies(stream: *std.io.FixedBufferStream([]const u8), allocator: std.mem.Allocator) !struct { usize, []const Dependency } {
    const reader = stream.reader();

    const offset = try reader.readInt(u32, .big);

    const start = try stream.getPos();
    defer stream.seekTo(start) catch unreachable;

    try stream.seekTo(offset);

    const dependencies = try allocator.alloc(Dependency, try reader.readInt(u32, .big));
    errdefer allocator.free(dependencies);
    for (dependencies) |*dependency| {
        dependency.* = .{
            .ident = switch (try reader.readByte()) {
                1 => .{
                    .hash = blk: {
                        var buf: [std.crypto.hash.Sha1.digest_length]u8 = undefined;
                        _ = try reader.readAll(&buf);
                        break :blk buf;
                    },
                },
                2 => .{ .guid = try reader.readInt(u32, .big) },
                else => |resource_type| std.debug.panic("Unknown resource type {d}", .{resource_type}),
            },
            .type = @enumFromInt(try reader.readInt(u32, .big)),
        };
    }

    return .{ offset, dependencies };
}
