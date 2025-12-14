use derive_debug::CustomDebug;

#[derive(CustomDebug)]
pub struct Field<T> {
    name: T,
    #[debug = "0b{:08b}"]
    bitmask: u8,
}

fn main() {
    let f = Field {
        name: "F",
        bitmask: 0b00011100,
    };

    println!("{:?}", f);
}
