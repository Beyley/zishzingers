const std = @import("std");

const StreamSource = @This();

const GetSeekPosError = error{};
const ReadError = error{};
const SeekError = error{};
const WriteError = std.mem.Allocator.Error || error{};

array_list: std.ArrayList(u8),
pos: u64,

pub const Reader = std.io.Reader(*StreamSource, ReadError, read);
pub const Writer = std.io.Writer(*StreamSource, WriteError, write);
pub const SeekableStream = std.io.SeekableStream(
    *StreamSource,
    SeekError,
    GetSeekPosError,
    seekTo,
    seekBy,
    getPos,
    getEndPos,
);

pub fn getEndPos(self: *StreamSource) GetSeekPosError!u64 {
    return self.array_list.items.len;
}

pub fn getPos(self: *StreamSource) GetSeekPosError!u64 {
    return self.pos;
}

pub fn read(self: *StreamSource, dest: []u8) ReadError!usize {
    //Get the amount of bytes to be read
    const amount_read: usize = @intCast(@min(dest.len, self.array_list.items.len - self.pos));

    //Copy the data into the output
    @memcpy(dest, self.array_list.items[self.pos .. self.pos + amount_read]);

    //Increment the position
    self.pos += @intCast(amount_read);

    //Return the amount of data read
    return amount_read;
}

pub fn seekBy(self: *StreamSource, amt: i64) SeekError!void {
    self.pos += amt;
}

pub fn seekTo(self: *StreamSource, pos: u64) SeekError!void {
    self.pos = pos;
}

pub fn write(self: *StreamSource, bytes: []const u8) WriteError!usize {
    const end_pos = self.pos + bytes.len;

    //Make sure all the data can fit
    try self.array_list.ensureTotalCapacity(end_pos);

    //Make sure the length of the slice is correct
    self.array_list.items.len = @max(end_pos, self.array_list.items.len);

    //Copy the data into the array list
    @memcpy(self.array_list.items[self.pos..end_pos], bytes);

    self.pos += bytes.len;

    return bytes.len;
}

pub fn reader(self: *StreamSource) Reader {
    return .{ .context = self };
}

pub fn writer(self: *StreamSource) Writer {
    return .{ .context = self };
}

pub fn seekableStream(self: *StreamSource) SeekableStream {
    return .{ .context = self };
}
