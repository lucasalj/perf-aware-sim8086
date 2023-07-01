pub struct Runtime {
    registers: [u16; 8],
}

impl Runtime {
    pub fn new() -> Self {
        Self {
            registers: [0u16; 8],
        }
    }
}
