//! By convention, root.zig is the root source file when making a library.
const std = @import("std");
const json = @import("json.zig");

// Export the json module for Zig users
pub const Json = json;

// Simple C API for benchmarking
// Returns 0 on success, 1 on error
export fn zig_json_parse_distinct_user_ids(data: [*c]const u8, len: usize, out_ids: [*c]u64, out_count: *usize, max_count: usize) c_int {
    _ = data;
    _ = len;
    _ = out_ids;
    _ = out_count;
    _ = max_count;
    return 0;
    // const allocator = std.heap.c_allocator;
    // const source = data[0..len];

    // const result = json.parse(u8, allocator, source);
    // defer result.deinit();

    // const string_buf = switch (result.res) {
    //     .root => result.string_buf.items,
    //     .err => return 1,
    // };

    // switch (result.res) {
    //     .root => |root| {
    //         var count: usize = 0;

    //         // Extract user IDs from tweets
    //         switch (root) {
    //             .object => |obj| {
    //                 // Find "statuses" array
    //                 for (obj.props.items) |prop| {
    //                     // Get the key string from the string buffer
    //                     const key = string_buf[prop.key.chars.off .. prop.key.chars.off + prop.key.chars.len];
    //                     if (std.mem.eql(u8, key, "statuses")) {
    //                         switch (prop.value) {
    //                             .array => |statuses| {
    //                                 // Process each tweet in the statuses array
    //                                 for (statuses.items.items) |status| {
    //                                     if (count >= max_count) break;

    //                                     switch (status) {
    //                                         .object => |tweet| {
    //                                             // Look for "user" and "retweeted_status" in tweet
    //                                             var found_user = false;
    //                                             var found_retweet = false;

    //                                             for (tweet.props.items) |tweet_prop| {
    //                                                 const tweet_key = string_buf[tweet_prop.key.chars.off .. tweet_prop.key.chars.off + tweet_prop.key.chars.len];

    //                                                 if (std.mem.eql(u8, tweet_key, "user") and !found_user) {
    //                                                     // Extract user.id
    //                                                     switch (tweet_prop.value) {
    //                                                         .object => |user| {
    //                                                             for (user.props.items) |user_prop| {
    //                                                                 const user_key = string_buf[user_prop.key.chars.off .. user_prop.key.chars.off + user_prop.key.chars.len];
    //                                                                 if (std.mem.eql(u8, user_key, "id")) {
    //                                                                     switch (user_prop.value) {
    //                                                                         .number => |id| {
    //                                                                             if (count < max_count) {
    //                                                                                 out_ids[count] = @intFromFloat(id.value);
    //                                                                                 count += 1;
    //                                                                                 found_user = true;
    //                                                                             }
    //                                                                         },
    //                                                                         else => {},
    //                                                                     }
    //                                                                 }
    //                                                             }
    //                                                         },
    //                                                         else => {},
    //                                                     }
    //                                                 } else if (std.mem.eql(u8, tweet_key, "retweeted_status") and !found_retweet) {
    //                                                     // Extract retweeted_status.user.id
    //                                                     switch (tweet_prop.value) {
    //                                                         .object => |retweet| {
    //                                                             for (retweet.props.items) |retweet_prop| {
    //                                                                 const retweet_key = string_buf[retweet_prop.key.chars.off .. retweet_prop.key.chars.off + retweet_prop.key.chars.len];
    //                                                                 if (std.mem.eql(u8, retweet_key, "user")) {
    //                                                                     switch (retweet_prop.value) {
    //                                                                         .object => |user| {
    //                                                                             for (user.props.items) |user_prop| {
    //                                                                                 const user_key = string_buf[user_prop.key.chars.off .. user_prop.key.chars.off + user_prop.key.chars.len];
    //                                                                                 if (std.mem.eql(u8, user_key, "id")) {
    //                                                                                     switch (user_prop.value) {
    //                                                                                         .number => |id| {
    //                                                                                             if (count < max_count) {
    //                                                                                                 out_ids[count] = @intFromFloat(id.value);
    //                                                                                                 count += 1;
    //                                                                                                 found_retweet = true;
    //                                                                                             }
    //                                                                                         },
    //                                                                                         else => {},
    //                                                                                     }
    //                                                                                 }
    //                                                                             }
    //                                                                         },
    //                                                                         else => {},
    //                                                                     }
    //                                                                 }
    //                                                             }
    //                                                         },
    //                                                         else => {},
    //                                                     }
    //                                                 }
    //                                             }
    //                                         },
    //                                         else => {},
    //                                     }
    //                                 }
    //                             },
    //                             else => {},
    //                         }
    //                         break; // Found statuses, no need to continue
    //                     }
    //                 }
    //             },
    //             else => return 1,
    //         }

    //         out_count.* = count;
    //         return 0;
    //     },
    //     .err => return 1,
    // }
}

// Simple benchmark: just parse and validate
export fn zig_json_parse_validate(data: [*c]const u8, len: usize) c_int {
    const allocator = std.heap.c_allocator;
    const source = data[0..len];

    const result = json.parse(.utf8, allocator, source);
    defer result.deinit();

    return switch (result.res) {
        .root => 0,
        .err => 1,
    };
}
