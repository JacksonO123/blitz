pub const Settings = struct {
    strictNumbers: bool,
};

pub fn getDefaultSettings() Settings {
    // TODO - later make number type assumptions from raw numbers
    // and insert cast nodes from number type mismatches
    return .{
        .strictNumbers = true,
    };
}
