use std::marker::PhantomData;

use derive_debug::CustomDebug;

#[derive(CustomDebug)]
pub struct Field<T, Y> {
    marker: PhantomData<Y>,
    name: T,
    #[debug = "0b{:08b}"]
    bitmask: u8,
}

struct NotDebug;

fn main() {
    let f = Field {
        marker: PhantomData::<NotDebug>,
        name: "F",
        bitmask: 0b00011100,
    };

    println!("{:?}", f);
}
