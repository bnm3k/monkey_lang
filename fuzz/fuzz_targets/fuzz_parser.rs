#![no_main]

use libfuzzer_sys::fuzz_target;

extern crate monkey;

fuzz_target!(|data: &[u8]| {
    // fuzzed code goes here
    if let Ok(s) = std::str::from_utf8(data) {
        let _ = monkey::parse_test_input(s);
    }
});
