const std = @import("std");

const MMTypes = @import("MMTypes.zig");
const Stream = @import("stream.zig");

pub const MMStream = Stream.MMStream(std.io.FixedBufferStream([]const u8));

pub const Resource = struct {
    stream: MMStream,
    allocator: std.mem.Allocator,
    type: MMTypes.ResourceType,

    pub fn deinit(self: Resource) void {
        self.allocator.free(self.stream.stream.buffer);
    }
};

pub fn writeResource(resource: Resource, allocator: std.mem.Allocator) !void {
    _ = resource; // autofix
    _ = allocator; // autofix
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

            if (revision.head >= 0x109) {
                dependency_table_offset = try raw_reader.readInt(u32, .big);
                // dependency_table_offset, const dependencies = try getDependencies(raw_reader, allocator);

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
            };
        },
        else => std.debug.panic("Unknown serialization method {s}\n", .{@tagName(serialization_method)}),
    }
}

// fn getDependencies(reader: anytype, allocator: std.mem.Allocator) !struct { usize, []const MMTypes.ResourceIdentifier } {
//     const offset = try reader.readInt(u32, .big);

//     const dependencies = try allocator.alloc(MMTypes.ResourceIdentifier, try reader.readInt(u32, .big));
//     errdefer allocator.free(dependencies);
//     for (dependencies) |*dependency| {
//         dependency.* = switch (try reader.readByte()) {
//             1 => .{ .hash = blk: {
//                 var buf: [std.crypto.hash.Sha1.digest_length]u8 = undefined;
//                 _ = try reader.readAll(&buf);
//                 break :blk buf;
//             } },
//             2 => .{ .guid = try reader.readInt(u32, .big) },
//             else => |resource_type| std.debug.panic("Unknown resource type {d}", .{resource_type}),
//         };
//     }

//     return .{ offset, dependencies };
// }
